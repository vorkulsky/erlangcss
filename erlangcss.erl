%% @author Anton Fedorov <vorkulsky@gmail.com>

-module(erlangcss).
-export([select/2]).

-define(ESCAPE_RE, <<"\\\\[^[:xdigit:]]|\\\\[[:xdigit:]]{1,6}">>).

-define(ATTR_RE_LEFT, <<"\\[((?:">>).

-define(ATTR_RE_RIGHT, <<"
  |[\\w\\-])+)  # Key
  (?:
    (\\W)?  # Operator
    =
    (?:\"((?:\\\\\"|[^\"])+)\"|(\\S+))  # Value
  )?
  \\]">>).

attr_re() ->
    [?ATTR_RE_LEFT, ?ESCAPE_RE, ?ATTR_RE_RIGHT].

-define(CLASS_ID_RE, <<"
  (?:
    (?:\\.((?:\\\\\\.|[^\\#.])+))  # Class
  |
    (?:\\#((?:\\\\\\#|[^.\\#])+))  # ID
  )">>).

-define(PSEUDO_CLASS_RE, <<"(?::([\\w\\-]+)(?:\\(((?:\\([^)]+\\)|[^)])+)\\))?)">>).

% TOKEN_RE captured parts: [selector, separator, element, pseudoclass, pseudoclass name, pseudoclass parameter,
%                   attribute, key, operation, value in quotes, unquoted value, combinator]

-define(TOKEN_RE_1, <<"
  \\s*(,\\s*)?  # Separator
  ((?:[^[\\\\:\\s,]|">>).

-define(TOKEN_RE_2, <<"
  \\s?)+)?  # Element
  (">>).

-define(TOKEN_RE_3, <<"
  *)?  # Pseudoclass
  ((?:">>).

-define(TOKEN_RE_4, <<"
  )*)?  # Attributes
  (?:
    \\s*
    ([>+~])  # Combinator
  )?">>).

token_re() ->
    [?TOKEN_RE_1, ?ESCAPE_RE, ?TOKEN_RE_2, ?PSEUDO_CLASS_RE, ?TOKEN_RE_3, attr_re(), ?TOKEN_RE_4].

select(Tree, Css) ->
    Pattern = compile(Css),
    select(Tree, Pattern, []).

select(_, [], Results) -> lists:reverse(Results);
select(Tree, [Head | Tail], Results) ->
    Result = lists:reverse(apply_selector(Tree, root, Head, [])),
    select(Tree, Tail, [Result | Results]).

apply_selector(Tree={Tag, _, Children}, Father, Pattern, Result) when not is_atom(Tag) ->
    apply_selector(Children, Tree, Pattern, corresponds_to_selector(Tree, Father, Pattern, []) ++ Result);
apply_selector([], _, _, Result) -> lists:reverse(Result);
apply_selector([Head | Tail], Father, Pattern, Result) ->
    case Head of
        {_, _, _} ->
            NewResult = apply_selector(Head, Father, Pattern, Result),
            apply_selector(Tail, Father, Pattern, NewResult);
        _ -> apply_selector(Tail, Father, Pattern, Result)
    end;
apply_selector(_, _, _, Result) -> Result.

corresponds_to_selector(_, _, [], Result) -> Result;
corresponds_to_selector(Tree, Father, [{combinator, <<" ">>} | Tail], Result) ->
    child(Tree, Father, Tail, Result, mediate);
corresponds_to_selector(Tree, Father, [{combinator, <<">">>} | Tail], Result) ->
    child(Tree, Father, Tail, Result, immediate);
corresponds_to_selector(Tree, Father, [{combinator, <<"~">>} | Tail], Result) ->
    sibling(Tree, Father, Tail, Result, mediate);
corresponds_to_selector(Tree, Father, [{combinator, <<"+">>} | Tail], Result) ->
    sibling(Tree, Father, Tail, Result, immediate);
corresponds_to_selector(Tree, Father, [Head={element, _, _, _} | Tail], Result) ->
    corresponds_to_selector(Tree, Father, Tail, corresponds_to_element(Tree, Father, Head) ++ Result).

child(Tree, Father, Tail, Result, _) -> [].

sibling(Tree, Father, Tail, Result, _) -> [].

corresponds_to_element(Element={Tag, Attribs, Children}, Father, {element, {tag, Name}, Pc, Attrs}) ->
    Bool = if
        Tag == <<"*">> -> true;
        true -> ignore_namespace_prefix(Tag, Name)
    end,
    case Bool of
        false -> false;
        true -> true
    end.

ignore_namespace_prefix(Tag, Tag) -> true;
ignore_namespace_prefix(Tag, Name) -> 
    Match = re:run(Tag, <<"(?:^|:)(.+)$">>, [global]),
     case Match of
        {match, Captured} ->
            Pred = fun([{}, Part]) -> Tag == subbinary(Name, Part) end,
            case lists:filter(Pred, Captured) of
                [] -> false;
                _ -> true
            end;
        _ -> false
    end.

compile(Css) ->
    BCss = iolist_to_binary(Css),
    Match = re:run(BCss, token_re(), [global, extended]),
    case Match of
        {match, Captured} -> compile_selector(BCss, Captured, [[]]);
        _ -> [[]]
    end.

compile_selector(_, [], Pattern) -> lists:reverse(lists:map(fun(X) -> lists:reverse(X) end, Pattern));
compile_selector(BCss, [Captured | Tail], Pattern) ->
    [Selector, Separator, Element, Pc, _, _, Attrs | CapturedTail] = Captured,
    Combinator = case CapturedTail of
        [_, _, _, _, C] -> subbinary(BCss, C);
        _ -> none
    end,
    case Selector of  % Trash
        {_, 0} -> compile_selector(BCss, Tail, Pattern);
        _ -> New_part = compile_part(subbinary(BCss, Element), subbinary(BCss, Pc), subbinary(BCss, Attrs), Combinator, []),
            case Separator of  % New selector
                {_, 0} -> [H|T] = Pattern,
                          compile_selector(BCss, Tail, [New_part ++ empty_combinator(H) | T]);
                _ -> compile_selector(BCss, Tail, [New_part | Pattern])
            end
    end.

empty_combinator(Pattern=[{element, _, _, _} | _]) -> [{combinator, <<" ">>} | Pattern];
empty_combinator(Pattern) -> Pattern.

compile_part(Element, Pc, Attrs, none, Pattern) ->
    [compile_element(Element, Pc, Attrs) | Pattern];
compile_part(Element, Pc, Attrs, Combinator, Pattern) ->
    [{combinator, Combinator}, compile_element(Element, Pc, Attrs) | Pattern].

% {element, tag, pc, attr}
compile_element(none, Pc, Attrs) ->
    {element, {tag, <<"*">>}, compile_pc(Pc), compile_attrs(Attrs)};
compile_element(Element, Pc, Attrs) ->
    {_, RE} = re:compile(<<"^(?:\\\\\\.|\\\\\\#|[^.#])+">>),
    Match = re:run(Element, RE),
    {Tag, Coi} = case Match of
        {match, [{Offset, Len}]} when Len > 0 -> {unescape(subbinary(Element, {Offset, Len})), subbinary(Element, Offset+Len)};
        _ -> {<<"*">>, Element}
    end,
    {element, {tag, Tag}, compile_pc(Pc), compile_class_or_id(Coi) ++ compile_attrs(Attrs)}.

compile_attrs(none) -> [];
compile_attrs(Attrs) ->
    Match = re:run(Attrs, attr_re(), [global, extended]),
    case Match of
        {match, Captured} -> attrs_parts(Attrs, Captured, []);
        _ -> []
    end.

attrs_parts(_, [], Pattern) -> lists:reverse(Pattern);
attrs_parts(Attrs, [[_, Key] | Tail], Pattern) ->
    attrs_parts(Attrs, Tail, [{attr, subbinary(Attrs, Key), none} | Pattern]);
attrs_parts(Attrs, [[_, Key, Op, Value] | Tail], Pattern) ->
    attrs_parts(Attrs, Tail, [{attr, subbinary(Attrs, Key), include_op(subbinary(Attrs, Op), subbinary(Attrs, Value))} | Pattern]);
attrs_parts(Attrs, [[_, Key, Op, _, Value] | Tail], Pattern) ->
    attrs_parts(Attrs, Tail, [{attr, subbinary(Attrs, Key), include_op(subbinary(Attrs, Op), subbinary(Attrs, Value))} | Pattern]).

compile_pc(none) -> [];
compile_pc(Pclasses) ->
    Match = re:run(Pclasses, ?PSEUDO_CLASS_RE, [global, extended]),
    case Match of
        {match, Captured} -> pc_parts(Pclasses, Captured, []);
        _ -> []
    end.

pc_parts(_, [], Pattern) -> lists:reverse(Pattern);
pc_parts(Pclasses, [[_, Name] | Tail], Pattern) ->
    pc_parts(Pclasses, Tail, [{pc, subbinary(Pclasses, Name), none} | Pattern]);
pc_parts(Pclasses, [[_, Name, Param] | Tail], Pattern) ->
    BinName = subbinary(Pclasses, Name),
    BinParam = subbinary(Pclasses, Param),
    case BinName of
        <<"not">> -> pc_parts(Pclasses, Tail, [{pc, BinName, lists:last(lists:last(compile(BinParam)))} | Pattern]);
        _ -> pc_parts(Pclasses, Tail, [{pc, BinName, BinParam} | Pattern])
    end.

compile_class_or_id(none) -> [];
compile_class_or_id(Coi) ->
    Match = re:run(Coi, ?CLASS_ID_RE, [global, extended]),
    case Match of
        {match, Captured} -> class_or_id_parts(Coi, Captured, []);
        _ -> []
    end.

class_or_id_parts(_, [], Pattern) -> lists:reverse(Pattern);
class_or_id_parts(Coi, [[_, Class] | Tail], Pattern) ->
    class_or_id_parts(Coi, Tail, [{attr, <<"class">>, include_op(<<"~">>, subbinary(Coi, Class))} | Pattern]);
class_or_id_parts(Coi, [[_, _, Id] | Tail], Pattern) ->
    class_or_id_parts(Coi, Tail, [{attr, <<"id">>, include_op(<<>>, subbinary(Coi, Id))} | Pattern]).

% ~= separated
% ^= prefix
% $= suffix
% *= contain
% = exact
include_op(_, none) -> none;
include_op(<<"~">>, Value) -> {separated, unescape(Value)};
include_op(<<"^">>, Value) -> {prefix, unescape(Value)};
include_op(<<"$">>, Value) -> {suffix, unescape(Value)};
include_op(<<"*">>, Value) -> {contain, unescape(Value)};
include_op(_, Value) -> {exact, unescape(Value)}.

subbinary(_, {_, 0}) -> none;
subbinary(Raw, {Offset, Len}) ->
    <<_:Offset/binary, Part:Len/binary, _/binary>> = Raw,
    Part;
subbinary(Raw, 0) -> Raw;
subbinary(Raw, Offset) ->
    <<_:Offset/binary, Part/binary>> = Raw,
    Part.

unescape(Value) ->
    to_bin(remove_backslash(unescape_unicode_characters(remove_escaped_newlines(Value)))).

to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(L) -> L.

remove_escaped_newlines(Value) ->
    {_, RE} = re:compile(<<"\\\\\\n">>),
    re:replace(Value, RE, <<>>, [global]).

remove_backslash(Value) ->
    {_, RE} = re:compile(<<"\\\\">>), %"
    re:replace(Value, RE, <<>>, [global]).
    
unescape_unicode_characters(Value) ->
    {_, RE} = re:compile(<<"\\\\([[:xdigit:]]{1,6})\\s*">>),
    Match = re:run(Value, RE, [global]),
    case Match of
        {match, Captured} -> convert_unicode_characters(Value, Captured, [[]], 0);
        _ -> Value
    end.

convert_unicode_characters(Value, [], Unescaped, _) -> 
    lists:reverse([Value | Unescaped]);
convert_unicode_characters(Value, [[{Offset, AllLen}, {_, Len}] | Tail], Unescaped, Shift) ->
    Numspaces = AllLen-Len-1,
    Pos = Offset-Shift,
    <<Prefix:Pos/binary, _:1/binary, Escapchar:Len/binary, _:Numspaces/binary, Suffix/binary>> = Value,
    Char = case charref(Escapchar) of
        undefined -> <<>>;
        Unichar -> unicode:characters_to_binary([Unichar])
    end,
    convert_unicode_characters(Suffix, Tail, [Char, Prefix | Unescaped], Shift+AllLen).

charref(Escapchar) ->
    try erlang:binary_to_integer(Escapchar, 16)
    catch
        error:badarg -> undefined
    end.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

regexp_test() ->
    ?assertEqual(
        re:run(<<"\\04Af">>, ?ESCAPE_RE),
        {match,[{0,5}]}),
    {_, Attr_re} = re:compile(attr_re(), [extended]),
    ?assertEqual(
        re:run(unicode:characters_to_list("[name~=\"foo\"]"), Attr_re),
        {match,[{0,13},{1,4},{5,1},{8,3}]}),
    ?assertEqual(
        re:run(<<"[\\01Af9*=foo]">>, Attr_re),
        {match,[{0,13},{1,6},{7,1},{-1,0},{9,3}]}),
    {_, Class_id_re} = re:compile(?CLASS_ID_RE, [extended]),
    ?assertEqual(
        re:run(<<".fo\\.o">>, Class_id_re),  % Class
        {match,[{0,6},{1,5}]}),
    ?assertEqual(
        re:run(<<"#fo\\#o">>, Class_id_re),  % ID
        {match,[{0,6},{-1,0},{1,5}]}),
    ?assertEqual(
        re:run(<<":nth-last-of-type(n)">>, ?PSEUDO_CLASS_RE),
        {match,[{0,20},{1,16},{18,1}]}),
    {_, Token_re} = re:compile(token_re(), [extended]),
    ?assertEqual(
        re:run(<<>>, Token_re),
        {match,[{0,0},{-1,0},{-1,0},{0,0},{-1,0},{-1,0},{0,0}]}),
    ?assertEqual(
        re:run(<<"input[name=\"foo\"]">>, Token_re),
        {match,[{0,17},{-1,0},{0,5},{5,0},{-1,0},{-1,0},{5,12},{6,4},{-1,0},{12,3}]}),
    ?assertEqual(
        re:run(<<"input[name=\"foo\"][title=\"a\"]">>, Token_re),
        {match,[{0,28},{-1,0},{0,5},{5,0},{-1,0},{-1,0},{5,23},{18,5},{-1,0},{25,1}]}),
    ?assertEqual(
        re:run(<<"div#page > div.logo, li[name~=\"foo\"] + a">>, Token_re, [global]),
        {match, [[{0,10},{-1,0},{0,8},{8,0},{-1,0},{-1,0},{8,0},{-1,0},{-1,0},{-1,0},{-1,0},{9,1}],
                [{10,9},{-1,0},{11,8},{19,0},{-1,0},{-1,0},{19,0}],
                [{19,19},{19,2},{21,2},{23,0},{-1,0},{-1,0},{23,13},{24,4},{28,1},{31,3},{-1,0},{37,1}],
                [{38,2},{-1,0},{39,1},{40,0},{-1,0},{-1,0},{40,0}],
                [{40,0},{-1,0},{-1,0},{40,0},{-1,0},{-1,0},{40,0}]]}),
    ok.

subbinary_test() ->
    ?assertEqual(
        subbinary(<<"ab">>, {-1,0}),
        none),
    ?assertEqual(
        subbinary(<<"abcd">>, {1,2}),
        <<"bc">>),
    ok.

unescape_unicode_characters_test() ->
    ?assertEqual(
        charref(<<"04Af">>),
        1199),
    ?assertEqual(
        charref(<<"04AT">>),
        undefined),
    ?assertEqual(
        to_bin(unescape_unicode_characters(<<"j\\04Af  d">>)),
        <<106,210,175,100>>),
    ?assertEqual(
        to_bin(unescape_unicode_characters(<<"\\0418\\0306">>)),
        <<208,152,204,134>>),
    ?assertEqual(
        to_bin(unescape_unicode_characters(<<"at">>)),
        <<"at">>),
    ok.

compile_attrs_test() ->
    ?assertEqual(
        compile_attrs(<<"[href]">>),
        [{attr, <<"href">>, none}]),
    ?assertEqual(
        compile_attrs(<<"[name~=\"foo\"]">>),
        [{attr, <<"name">>, {separated, <<"foo">>}}]),
    ?assertEqual(
        compile_attrs(<<"[name~=foo]">>),
        [{attr, <<"name">>, {separated, <<"foo">>}}]),
     ?assertEqual(
        compile_attrs(<<"[name^=\"foo\"][title*=a]">>),
        [{attr, <<"name">>, {prefix, <<"foo">>}}, {attr, <<"title">>, {contain, <<"a">>}}]),
    ok.

compile_pc_test() ->
    ?assertEqual(
        compile_pc(<<":empty">>),
        [{pc, <<"empty">>, none}]),
    ?assertEqual(
        compile_pc(<<":nth-last-of-type(n)">>),
        [{pc, <<"nth-last-of-type">>, <<"n">>}]),
    ?assertEqual(
        compile_pc(<<":nth-last-of-type(n):empty">>),
        [{pc, <<"nth-last-of-type">>, <<"n">>}, {pc, <<"empty">>, none}]),
    ok.

compile_class_or_id_parts_test() ->
    ?assertEqual(
        compile_class_or_id(<<".fo\\.o">>),
        [{attr, <<"class">>, {separated, <<"fo.o">>}}]),
    ?assertEqual(
        compile_class_or_id(<<"#fo\\#o">>),
        [{attr, <<"id">>, {exact, <<"fo#o">>}}]),
    ok.

compile_element_test() ->
    ?assertEqual(
        compile_element(<<"div#page">>, <<":empty">>, <<"[name~=\"foo\"]">>),
        {element, {tag, <<"div">>}, [{pc, <<"empty">>, none}],
            [{attr, <<"id">>, {exact, <<"page">>}}, {attr, <<"name">>, {separated, <<"foo">>}}]}),
    ?assertEqual(
        compile_element(<<"div">>, <<":empty">>, <<"[name~=\"foo\"]">>),
        {element, {tag, <<"div">>}, [{pc, <<"empty">>, none}], [{attr, <<"name">>, {separated, <<"foo">>}}]}),
    ?assertEqual(
        compile_element(<<".page">>, <<":empty">>, <<"[name~=\"foo\"]">>),
        {element, {tag, <<"*">>}, [{pc, <<"empty">>, none}],
            [{attr, <<"class">>, {separated, <<"page">>}}, {attr, <<"name">>, {separated, <<"foo">>}}]}),
    ?assertEqual(
        compile_element(<<>>, <<>>, <<>>),
        {element, {tag, <<"*">>}, [], []}),
    ok.

compile_test() ->
    ?assertEqual(
        compile(<<>>),
        [[]]),
    ?assertEqual(
        compile(<<"*">>),
        [[{element, {tag, <<"*">>}, [], []}]]),
    ?assertEqual(
        compile(<<":empty">>),
        [[{element, {tag, <<"*">>}, [{pc, <<"empty">>, none}], []}]]),
    ?assertEqual(
        compile(<<"a[href]">>),
        [[{element, {tag, <<"a">>}, [], [{attr, <<"href">>, none}]}]]),
    ?assertEqual(
        compile(<<"div[a=\"0\"][b=\"4\"]">>),
        [[{element, {tag, <<"div">>}, [], [{attr, <<"a">>, {exact, <<"0">>}}, {attr, <<"b">>, {exact, <<"4">>}}]}]]),
    ?assertEqual(
        compile(<<"p:not(.foo)">>),
        [[{element, {tag, <<"p">>}, [{pc, <<"not">>, 
            {element, {tag, <<"*">>}, [], [{attr, <<"class">>, {separated, <<"foo">>}}]}
        }], []}]]),
    ?assertEqual(
        compile(<<"p:nth-of-type(3)[a=\"c\"]">>),
        [[{element, {tag, <<"p">>}, [{pc, <<"nth-of-type">>, <<"3">>}], [{attr, <<"a">>, {exact, <<"c">>}}]}]]),
    ?assertEqual(
        compile(<<"div p:only-of-type">>),
        [[
            {element, {tag, <<"div">>}, [], []},
            {combinator, <<" ">>},
            {element, {tag, <<"p">>}, [{pc, <<"only-of-type">>, none}], []}
        ]]),
    ?assertEqual(
        compile(<<"p[a=\"c\"]:empty">>),
        [[
            {element, {tag, <<"p">>}, [], [{attr, <<"a">>, {exact, <<"c">>}}]},
            {combinator, <<" ">>},
            {element, {tag, <<"*">>}, [{pc, <<"empty">>, none}], []}
        ]]),
    ?assertEqual(
        compile(<<"div#page > div.logo, li[name~=\"foo\"] + a">>),
        [[
                {element, {tag, <<"div">>}, [], [{attr, <<"id">>, {exact, <<"page">>}}]},
                {combinator, <<">">>},
                {element, {tag, <<"div">>}, [], [{attr, <<"class">>, {separated, <<"logo">>}}]}
            ],[
                {element, {tag, <<"li">>}, [], [{attr, <<"name">>, {separated, <<"foo">>}}]},
                {combinator, <<"+">>},
                {element, {tag, <<"a">>}, [], []}
        ]]),
    ok.

-endif.
