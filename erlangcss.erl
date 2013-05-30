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
    recover_selected(select(prepare_tree(Tree), Pattern, [])).

prepare_tree({Tag, Attrs, Children}) when not is_atom(Tag) ->
    {Tag, make_ref(), Attrs, prepare_tree(Children, [])};
prepare_tree(Tree) -> Tree.

prepare_tree([], Result) -> lists:reverse(Result);
prepare_tree([Head | Tail], Result) ->
    prepare_tree(Tail, [prepare_tree(Head) | Result]).

recover_selected(Selected) -> recover_selected(Selected, []).

recover_selected([], Result) -> lists:reverse(Result);
recover_selected([Head | Tail], Result) ->
    recover_selected(Tail, [recover_tree(Head, []) | Result]).

recover_tree({Tag, _, Attrs, Children}) ->
    {Tag, Attrs, recover_tree(Children, [])};
recover_tree(Tree) -> Tree.

recover_tree([], Result) -> lists:reverse(Result);
recover_tree([Head | Tail], Result) ->
    recover_tree(Tail, [recover_tree(Head) | Result]).

select(_, [], Results) -> lists:reverse(Results);
select(Tree, [Head | Tail], Results) ->
    Result = lists:reverse(apply_selector(Tree, [root], lists:reverse(Head), [])),
    select(Tree, Tail, [Result | Results]).

apply_selector(Node={_, _, _, Children}, Fathers, Pattern, Result) ->
    case corresponds_to_selector(Node, Fathers, Pattern) of
        true -> apply_selector(Children, [Node | Fathers], Pattern, [Node | Result]);
        false -> apply_selector(Children, [Node | Fathers], Pattern, Result)
    end;
apply_selector([Head | Tail], Fathers, Pattern, Result) ->
    apply_selector(Tail, Fathers, Pattern, apply_selector(Head, Fathers, Pattern, Result));
apply_selector(_, _, _, Result) -> Result.

corresponds_to_selector(_, _, []) -> true;
corresponds_to_selector(_, Fathers, [{combinator, <<" ">>} | Tail]) ->
    ancestor(Fathers, Tail);
corresponds_to_selector(_, Fathers, [{combinator, <<">">>} | Tail]) ->
    parent(Fathers, Tail);
corresponds_to_selector(Node, Fathers, [{combinator, <<"~">>} | Tail]) ->
    sibling(Node, Fathers, Tail, mediate);
corresponds_to_selector(Node, Fathers, [{combinator, <<"+">>} | Tail]) ->
    sibling(Node, Fathers, Tail, immediate);
corresponds_to_selector(Node, Fathers, [Head={element, _, _, _} | Tail]) ->
    corresponds_to_element(Node, Fathers, Head) andalso corresponds_to_selector(Node, Fathers, Tail).

ancestor([root], _) -> false;
ancestor([Father | Fathers], Pattern) ->
    corresponds_to_selector(Father, Fathers, Pattern) orelse ancestor(Fathers, Pattern).

parent([root], _) -> false;
parent([Father | Fathers], Pattern) ->
    corresponds_to_selector(Father, Fathers, Pattern).

sibling(_, [root], _, _) -> false;
sibling({_, Ref, _, _}, Fathers=[{_, _, _, Children} | _], Pattern, Nearness) ->
    run_by_children(Ref, Fathers, Children, Pattern, Nearness, false).

run_by_children(Ref, _, [{_, Ref, _, _} | _], _, _, Found) -> Found;
run_by_children(Ref, Fathers, [Child={_, _, _, _} | Children], Pattern, immediate, _) ->
    run_by_children(Ref, Fathers, Children, Pattern, immediate, corresponds_to_selector(Child, Fathers, Pattern));
run_by_children(Ref, Fathers, [Child={_, _, _, _} | Children], Pattern, mediate, Found) ->
    corresponds_to_selector(Child, Fathers, Pattern) orelse run_by_children(Ref, Fathers, Children, Pattern, mediate, Found);
run_by_children(Ref, Fathers, [_ | Children], Pattern, Nearness, Found) ->
    run_by_children(Ref, Fathers, Children, Pattern, Nearness, Found).

corresponds_to_element(Element={Tag, _, Attribs, _}, Fathers, Pattern={element, {tag, Name}, _, Attrs}) ->
    if
        Name == <<"*">> -> true;
        true -> ignore_namespace_prefix(Tag, Name)
    end andalso corresponds_to_pc(Element, Fathers, Pattern) andalso corresponds_to_attrs(Attribs, Attrs).

ignore_namespace_prefix(Name, Name) -> true;
ignore_namespace_prefix(Name, Pattern) -> 
    case re:run(Name, <<":(.+)$">>) of
        {match, [_, Part]} -> subbinary(Name, Part) == Pattern;
        _ -> false
    end.

corresponds_to_attrs(_, []) -> true;
corresponds_to_attrs(Attrs, [Head | Tail]) ->
    corresponds_to_attr(Attrs, Head) andalso corresponds_to_attrs(Attrs, Tail).

corresponds_to_attr([], _) -> false;
corresponds_to_attr([Head | Tail], Pattern) ->
    corresponds_to_attr(Head, Pattern) orelse corresponds_to_attr(Tail, Pattern);
corresponds_to_attr({Name, Value}, {attr, PName, PValue}) ->
    ignore_namespace_prefix(Name, PName) andalso corresponds_to_value(Value, PValue).

corresponds_to_value(_, none) -> true;
corresponds_to_value(Value, {exact, PValue}) -> Value == PValue;
corresponds_to_value(Value, {prefix, PValue}) ->
    case binary:match(Value, PValue) of
        {0, _} -> true;
        _ -> false
    end;
corresponds_to_value(Value, {suffix, PValue}) ->
    case binary:match(Value, PValue) of
        {Pos, Len} -> Pos+Len == byte_size(Value);
        _ -> false
    end;
corresponds_to_value(Value, {contain, PValue}) ->
    case binary:match(Value, PValue) of
        nomatch -> false;
        _ -> true
    end;
corresponds_to_value(Value, {separated, PValue}) ->
    case binary:match(Value, PValue) of
        {Pos, Len} -> Pos == 0 orelse Pos+Len == byte_size(Value) orelse
                      is_whitespace(Value, Pos-1) orelse
                      is_whitespace(Value, Pos+Len);
        _ -> false
    end.

is_whitespace(Value, Pos) ->
    Char = binary:part(Value, Pos, 1),
    Match = re:run(Char, <<"\s">>),
    case Match of
        {match, _} -> true;
        _ -> false
    end.
    
corresponds_to_pc(Element, Fathers, {element, _, Pc, _}) ->
    corresponds_to_pcs(Element, Fathers, Pc).

corresponds_to_pcs(_, _, []) -> true;
corresponds_to_pcs(Element, Fathers, [{pc, Class, Args} | Tail]) ->
    first_filter(Element, Fathers, Class, Args) andalso corresponds_to_pcs(Element, Fathers, Tail).

first_filter(Element, Fathers, Class, Args) ->
    Match = re:run(Class, <<"^first-(?:(child)|of-type)$">>),
    case Match of
        {match, [_, {_, 0}]} -> pc_selection(Element, Fathers, <<"nth-of-type">>, <<"1">>);
        {match, _} -> pc_selection(Element, Fathers, <<"nth-child">>, <<"1">>);
        _ -> last_filter(Element, Fathers, Class, Args)
    end.

last_filter(Element, Fathers, Class, Args) ->
    Match = re:run(Class, <<"^last-(?:(child)|of-type)$">>),
    case Match of
        {match, [_, {_, 0}]} -> pc_selection(Element, Fathers, <<"nth-last-of-type">>, <<"-n+1">>);
        {match, _} -> pc_selection(Element, Fathers, <<"nth-last-child">>, <<"-n+1">>);
        _ -> pc_selection(Element, Fathers, Class, Args)
    end.

pc_selection({_, _, Attribs, _}, _, <<"checked">>, _) ->
    corresponds_to_attr(Attribs, {attr, <<"checked">>, none}) orelse
    corresponds_to_attr(Attribs, {attr, <<"selected">>, none});
pc_selection({_, _, _, Children}, _, <<"empty">>, _) -> is_zero_length(Children);
pc_selection(_, [root], <<"root">>, _) -> true;
pc_selection(_, _, <<"root">>, _) -> false;
pc_selection(Element, Fathers, <<"not">>, Args) -> not corresponds_to_selector(Element, Fathers, [Args]);
pc_selection(Element, Fathers, Class, Args) -> pc_nth(Element, Fathers, Class, Args).

is_zero_length([]) -> true;
is_zero_length([Head | Tail]) ->
    is_zero_length(Head) andalso is_zero_length(Tail);
is_zero_length({comment, _}) -> true;
is_zero_length(_) -> false.

pc_nth(Element={_, Ref, _, _}, Fathers, Class, Args) ->
    Match = re:run(Class, <<"^nth-">>),
    case Match of
        {match, _} -> 
            Pair = equation(Args),
            Siblings = get_siblings(Element, Fathers, Class),
            RSiblings = 
                case re:run(Class, <<"^nth-last">>) of
                    {match, _} -> lists:reverse(Siblings);
                    _ -> Siblings
                end,
            find_itself_nth(Ref, RSiblings, length(RSiblings), 0, 0, Pair);
        _ -> pc_only(Element, Fathers, Class, Args)
    end.

get_siblings(Element, [root], _) -> [Element];
get_siblings({Type, _, _, _}, [{_, _, _, Children} | _], Class) ->
    case re:run(Class, <<"of-type$">>) of
        {match, _} -> get_siblings_list(Children, Type, []);
        _ -> get_siblings_list(Children, notype, [])
    end.

get_siblings_list([], _, Result) -> lists:reverse(Result);
get_siblings_list([Head | Tail], Type, Result) ->
    case is_sibling(Head, Type) of
        true -> get_siblings_list(Tail, Type, [Head | Result]);
        false -> get_siblings_list(Tail, Type, Result)
    end.

is_sibling({_, _, _, _}, notype) -> true;
is_sibling({Type, _, _, _}, Type) -> true;
is_sibling({_, _, _, _}, _) -> false;
is_sibling(_, _) -> false.

find_itself_nth(Ref, Siblings, Len, Counter, Shift, Pair={A, B}) ->
    Result = A*Counter + B,
    if
        Result < 1 -> find_itself_nth(Ref, Siblings, Len, Counter+1, Shift, Pair);
        Result-1 > Len -> false;
        true -> N = Result-Shift,
            [Sibling | Tail] = lists:nthtail(N-1, Siblings),
            case Sibling of
                {Ref, _, _, _} -> true;
                _ -> find_itself_nth(Ref, Tail, Len-N-1, Counter+1, Shift+N, Pair)
            end
    end.

equation(Equation) ->
    case re:run(Equation, <<"^even$">>, [caseless]) of
        {match, _} -> {2, 2};
        _ -> case re:run(Equation, <<"^odd$">>, [caseless]) of
                {match, _} -> {2, 1};
                _ -> {_, RE} = re:compile(<<"(?:(-?(?:\\d+)?)?(n))?\\s*\\+?\\s*(-?\\d+)?\\s*$">>, [caseless]),
                     {match, Captured} = re:run(Equation, RE),
                     analyze_captured(Equation, Captured)
             end
    end.

analyze_captured(_, [{_, 0}]) -> {0, 0};
analyze_captured(Equation, [_, A, B]) -> {analyze_captured_first(Equation, A, B), 0};
analyze_captured(Equation, [_, A, B, C]) ->
    {analyze_captured_first(Equation, A, B), analyze_captured_second(Equation, C)}.

analyze_captured_first(_, {_, 0}, {_, 0}) -> 0;
analyze_captured_first(_, {_, 0}, {_, 1}) -> 1;
analyze_captured_first(Equation, A, {_, 1}) ->
    case binary:part(Equation, A) of
        <<"-">> -> -1;
        Part -> list_to_integer(binary_to_list(Part))
    end.

analyze_captured_second(Equation, C) ->
    list_to_integer(binary_to_list(binary:part(Equation, C))).

pc_only(Element, Fathers, Class, Args) ->
    Match = re:run(Class, <<"^only-(?:child|(of-type))$">>),
    case Match of
        {match, [_, {_, 0}]} -> true;
        {match, _} -> true;
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
        {match, [{Pos, Len}]} when Len > 0 -> {unescape(subbinary(Element, {Pos, Len})), subbinary(Element, Pos+Len)};
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
subbinary(Raw, {Pos, Len}) ->
    binary:part(Raw, {Pos, Len});
subbinary(Raw, 0) -> Raw;
subbinary(Raw, Pos) ->
    <<_:Pos/binary, Part/binary>> = Raw,
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
convert_unicode_characters(Value, [[{Pos, AllLen}, {_, Len}] | Tail], Unescaped, Shift) ->
    Numspaces = AllLen-Len-1,
    Offset = Pos-Shift,
    <<Prefix:Offset/binary, _:1/binary, Escapchar:Len/binary, _:Numspaces/binary, Suffix/binary>> = Value,
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
    ?assertEqual(
        compile(<<"div *">>),
        [[
            {element, {tag, <<"div">>}, [], []},
            {combinator, <<" ">>},
            {element, {tag, <<"*">>}, [], []}
        ]]),
    ok.

corresponds_to_element_test() ->
    ?assertEqual(
        corresponds_to_element({<<"html">>, make_ref(), [], []}, root, {element, {tag, <<"*">>}, [], []}),
        true),
    ok.

select_by_tag_test() ->
    Tree = mochiweb_html:parse(<<"<html><div><p id=a></p></div><p id=b></p></html>">>),
    Div = mochiweb_html:parse(<<"<div><p id=a></p></div>">>),
    P1 = mochiweb_html:parse(<<"<p id=a></p>">>),
    P2 = mochiweb_html:parse(<<"<p id=b></p>">>),
    ?assertEqual(select(Tree, <<"p">>),[[P1, P2]]),
    ?assertEqual(select(Tree, <<"*">>),[[Tree, Div, P1, P2]]),
    ok.

select_by_attrs_test() ->
    Tree = mochiweb_html:parse(<<"<html><p id=a class=a data=\"d1 a d2\"></p><p id=b class=\"b c\" t data=\"d1 d3\"></p></html>">>),
    P1 = mochiweb_html:parse(<<"<p id=a class=a data=\"d1 a d2\"></p>">>),
    P2 = mochiweb_html:parse(<<"<p id=b class=\"b c\" t data=\"d1 d3\"></p>">>),
    ?assertEqual(select(Tree, <<"p#a">>), [[P1]]),
    ?assertEqual(select(Tree, <<"p.c">>), [[P2]]),
    ?assertEqual(select(Tree, <<"p[t]">>), [[P2]]),
    ?assertEqual(select(Tree, <<"p[data=\"d1 d3\"]">>), [[P2]]),
    ?assertEqual(select(Tree, <<"p[data^=\"d\"]">>), [[P1, P2]]),
    ?assertEqual(select(Tree, <<"p[data$=\"2\"]">>), [[P1]]),
    ?assertEqual(select(Tree, <<"p[data*=\"1\"]">>), [[P1, P2]]),
    ?assertEqual(select(Tree, <<"p[data~=\"a\"][data~=\"d1\"][data~=\"d2\"]">>), [[P1]]),
    ?assertEqual(select(Tree, <<"p.c[data~=\"d1\"]">>), [[P2]]),
    ok.

select_by_combinators_test() ->
    Tree = mochiweb_html:parse(<<"<html><div><p><a></a></p>text1<div></div>text2<p>text3</p></div><p></p><div><br/></div></html>">>),
    P1 = mochiweb_html:parse(<<"<p><a></a></p>">>),
    A = {<<"a">>, [], []},
    Div2 = {<<"div">>, [], []},
    P2 = mochiweb_html:parse(<<"<p>text3</p>">>),
    Div3 = mochiweb_html:parse(<<"<div><br/></div>">>),
    Br = {<<"br">>, [], []},
    ?assertEqual(select(Tree, <<"div > p">>), [[P1, P2]]),
    ?assertEqual(select(Tree, <<"div > *">>), [[P1, Div2, P2, Br]]),
    ?assertEqual(select(Tree, <<"div *">>), [[P1, A, Div2, P2, Br]]),
    ?assertEqual(select(Tree, <<"html ~ *">>), [[]]),
    ?assertEqual(select(Tree, <<"p ~ *">>), [[Div2, P2, Div3]]),
    ?assertEqual(select(Tree, <<"p + div">>), [[Div2, Div3]]),
    ?assertEqual(select(Tree, <<"html div p">>), [[P1, P2]]),
    ?assertEqual(select(Tree, <<"div ~ * > br">>), [[Br]]),
    ?assertEqual(select(Tree, <<"div ~ * + *">>), [[Div3]]),
    ok.

select_by_separators_test() ->
    Tree = mochiweb_html:parse(<<"<html><div><p></p></div><div><a></a></div></html>">>),
    ?assertEqual(
        select(Tree, <<"div p, a, div">>),
        [
            [{<<"p">>,[],[]}],
            [{<<"a">>,[],[]}],
            [{<<"div">>,[],[{<<"p">>,[],[]}]}, {<<"div">>,[],[{<<"a">>,[],[]}]}]
        ]),
    ok.

equation_test() ->
    ?assertEqual(equation(<<"even">>), {2, 2}),
    ?assertEqual(equation(<<"odd">>), {2, 1}),
    ?assertEqual(equation(<<"">>), {0, 0}),
    ?assertEqual(equation(<<"abraca">>), {0, 0}),
    ?assertEqual(equation(<<"-2">>), {0, -2}),
    ?assertEqual(equation(<<"n">>), {1, 0}),
    ?assertEqual(equation(<<"-n">>), {-1, 0}),
    ?assertEqual(equation(<<"-4n+8">>), {-4, 8}),
    ?assertEqual(equation(<<"4n-8">>), {4, -8}),
    ok.

find_itself_nth_test() ->
    Ref = make_ref(),
    Siblings = [{make_ref(),a1,b1,c1}, {Ref,a2,b2,c2}, {make_ref(),a3,b3,c3}, {make_ref(),a4,b4,c4}],
    ?assertEqual(
        find_itself_nth(Ref, Siblings, length(Siblings), 0, 0, {2, 2}),
        true
    ),
    ?assertEqual(
        find_itself_nth(Ref, Siblings, length(Siblings), 0, 0, {2, 1}),
        false
    ),
    ?assertEqual(
        find_itself_nth(Ref, Siblings, length(Siblings), 0, 0, {0, 2}),
        true
    ),
    ?assertEqual(
        find_itself_nth(Ref, Siblings, length(Siblings), 0, 0, {0, 3}),
        false
    ),
    ?assertEqual(
        find_itself_nth(Ref, Siblings, length(Siblings), 0, 0, {-3, 8}),
        true
    ),
    ok.

select_by_pc_test() ->
    ?assertEqual(
        select(mochiweb_html:parse(<<"<html><p checked></p><p selected></p><p></p></html>">>), <<":checked">>),
        [[
            {<<"p">>,[{<<"checked">>,<<"checked">>}],[]},
            {<<"p">>,[{<<"selected">>,<<"selected">>}],[]}
        ]]),
    ?assertEqual(
        select(mochiweb_html:parse(<<"<html><div><p></p></div><b><!--A--><!--B--></b></html>">>), <<":empty">>),
        [[
            {<<"p">>,[],[]},
            {<<"b">>,[],[{comment,<<"A">>},{comment,<<"B">>}]}
        ]]),
    Tree3 = mochiweb_html:parse(<<"<html><br/></html>">>),
    ?assertEqual(select(Tree3, <<":root">>), [[Tree3]]),
    Tree4 = mochiweb_html:parse(<<"<html><p id=\"a\"></p><p id=\"b\"></p><p></p></html>">>),
    ?assertEqual(
        select(Tree4, <<"p:not(#a)">>),
        [[
            {<<"p">>,[{<<"id">>,<<"b">>}],[]},
            {<<"p">>,[],[]}
        ]]),
    ?assertEqual(
        select(Tree4, <<"p:first-child">>),
        [[
            {<<"p">>,[{<<"id">>,<<"a">>}],[]}
        ]]),
    ?assertEqual(
        select(Tree4, <<"p:nth-child(2n-1)">>),
        [[
            {<<"p">>,[{<<"id">>,<<"a">>}],[]}
        ]]),
    ?assertEqual(
        select(Tree4, <<"p:nth-child(odd)">>),
        [[
            {<<"p">>,[{<<"id">>,<<"a">>}],[]}
        ]]),
    ok.

-endif.
