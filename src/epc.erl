-module(epc).

-type parse_result(T) :: {ok, T, binary()} | {error, string()}.
-type parser(T) :: fun((binary()) -> parse_result(T)).

-export([char/1, sequence/2, choice/1, choice/2, parse/2, string/1, digit/0, map/2, many/1, many1/1, lazy/1, sep_by/2, satisfy/1, optional/1, none_of/1, spaces/0, token/1]).


-doc "Parse a specific character.".
-spec char(char()) -> parser(char()).
char(Target) ->
    %% Match a single UTF-8 character or byte from binary
    fun(<<H/utf8, Rest/binary>>) when H =:= Target -> {ok, H, Rest};
       (_) -> {error, "Unexpected character"}
    end.


-doc "Parse a specific string.".
-spec string(binary()) -> parser(binary()).
string(Target) when is_binary(Target) ->
    Size = byte_size(Target),
    fun(Input) ->
            case Input of
                %% Fast prefix matching using bit syntax
                <<Target:Size/binary, Rest/binary>> -> {ok, Target, Rest};
                _ -> {error, "Expected string"}
            end
    end.


-doc "Parse a digit character.".
-spec digit() -> parser(char()).
digit() ->
    fun(<<H/utf8, Rest/binary>>) when H >= $0, H =< $9 -> {ok, H, Rest};
       (_) -> {error, "Expected digit"}
    end.


-doc "Transform the result of a parser.".
-spec map(parser(T), fun((T) -> U)) -> parser(U).
map(P, F) ->
    fun(Input) ->
            maybe
                {ok, Result, Rest} ?= P(Input),
                {ok, F(Result), Rest}
            else
                {error, Reason} -> {error, Reason}
            end
    end.


-doc "Parse zero or more occurrences of P using tail recursion.".
-spec many(parser(T)) -> parser([T]).
many(P) ->
    fun(Input) ->
            %% Use a named function for tail-recursive looping
            Loop = fun Loop(CurrentInput, Acc) ->
                           case P(CurrentInput) of
                               {ok, Result, Rest} ->
                                   Loop(Rest, [Result | Acc]);
                               {error, _} ->
                                   %% Reverse the accumulator to maintain original order
                                   {ok, lists:reverse(Acc), CurrentInput}
                           end
                   end,
            Loop(Input, [])
    end.


-doc "Combine two parsers to run in sequence.".
-spec sequence(parser(T), parser(U)) -> parser({T, U}).
sequence(P1, P2) ->
    fun(Input) ->
            maybe
                {ok, R1, Rest1} ?= P1(Input),
                {ok, R2, Rest2} ?= P2(Rest1),
                {ok, {R1, R2}, Rest2}
            else
                {error, Reason} -> {error, Reason}
            end
    end.


-doc "Try the first parser, if it fails, try the second.".
-spec choice(parser(T), parser(T)) -> parser(T).
choice(P1, P2) ->
    fun(Input) ->
            case P1(Input) of
                {ok, R, Rest} -> {ok, R, Rest};
                {error, _} -> P2(Input)
            end
    end.


-doc "Try a list of parsers in order, returning the result of the first one that succeeds.".
-spec choice([parser(T)]) -> parser(T).
choice(Parsers) ->
    fun(Input) ->
            choice_loop(Parsers, Input)
    end.


%% Internal helper loop for choice/1
choice_loop([], _) ->
    {error, "No parsers succeeded in choice"};
choice_loop([P | Rest], Input) ->
    case P(Input) of
        {ok, Result, RestInput} -> {ok, Result, RestInput};
        {error, _} -> choice_loop(Rest, Input)
    end.


-doc "Delay the evaluation of a parser, useful for recursive parsers.".
-spec lazy(fun(() -> parser(T))) -> parser(T).
lazy(ParserFun) ->
    fun(Input) ->
            Parser = ParserFun(),
            Parser(Input)
    end.


-doc "Parse zero or more occurrences of P, separated by Sep.".
-spec sep_by(parser(T), parser(atom())) -> parser([T]).
sep_by(P, Sep) ->
    %% Try to parse at least one P, followed by many (Sep followed by P)
    %% If that fails, return an empty list.
    choice(
      map(
        sequence(P, many(sequence(Sep, P))),
        fun({First, Rest}) ->
                %% Rest is a list of {SepResult, PResult} tuples
                [First | [ R || {_, R} <- Rest ]]
        end),
      %% Fallback to matching nothing (empty list)
      fun(Input) -> {ok, [], Input} end).


-doc "Parse a character that satisfies a predicate.".
-spec satisfy(fun((char()) -> boolean())) -> parser(char()).
satisfy(Pred) ->
    fun(<<H/utf8, Rest/binary>>) ->
            case Pred(H) of
                true -> {ok, H, Rest};
                false -> {error, "Predicate failed"}
            end;
       (<<>>) -> {error, "Unexpected end of input"}
    end.


-doc "Parse one or more occurrences of P.".
-spec many1(parser(T)) -> parser([T]).
many1(P) ->
    map(
      sequence(P, many(P)),
      fun({First, Rest}) -> [First | Rest] end).


-doc "Try to parse P. If it fails, return undefined without consuming input.".
-spec optional(T) -> T
              when T :: parser(T).
optional(P) ->
    choice(
      P,
      fun(Input) -> {ok, undefined, Input} end).


-doc "Parse any character not in the given list.".
-spec none_of(string()) -> parser(char()).
none_of(Chars) ->
    satisfy(fun(C) -> not lists:member(C, Chars) end).


-doc "Parse zero or more whitespace characters.".
-spec spaces() -> parser(binary()).
spaces() ->
    many(satisfy(fun(C) ->
                         C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r
                 end)).


-doc "Parse P, ignoring trailing spaces.".
-spec token(parser(T)) -> parser(T).
token(P) ->
    map(
      sequence(P, spaces()),
      fun({Result, _}) -> Result end).


-doc "Helper to run a parser.".
-spec parse(parser(T), binary()) -> parse_result(T).
parse(Parser, Input) ->
    Parser(Input).


%%  eunit testing functions
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


char_test() ->
    Parser = char($a),
    %% Success case
    ?assertEqual({ok, $a, ~"bc"}, Parser(~"abc")),
    %% Failure case
    ?assertEqual({error, "Unexpected character"}, Parser(~"bbc")).


sequence_test() ->
    P = sequence(char($a), char($b)),
    %% Success case
    ?assertEqual({ok, {$a, $b}, ~"c"}, P(~"abc")),
    %% Failure case (first fails)
    ?assertEqual({error, "Unexpected character"}, P(~"bbc")),
    %% Failure case (second fails)
    ?assertEqual({error, "Unexpected character"}, P(~"acb")).


choice_test() ->
    P = choice(char($a), char($b)),
    %% Match first
    ?assertEqual({ok, $a, ~"c"}, P(~"ac")),
    %% Match second
    ?assertEqual({ok, $b, ~"c"}, P(~"bc")),
    %% Match neither
    ?assertEqual({error, "Unexpected character"}, P(~"cc")).


digit_to_int_test() ->
    %% Combine digit, many, and map to parse an integer
    IntParser = map(many(digit()), fun(Ds) -> list_to_integer(Ds) end),
    ?assertEqual({ok, 123, ~"abc"}, IntParser(~"123abc")).


string_test() ->
    P = string(~"hello"),
    ?assertEqual({ok, ~"hello", ~" world"}, P(~"hello world")).


many_test() ->
    Parser = many(char($a)),
    %% Match multiple characters (many returns a list, so expect "aaa" not ~"aaa")
    ?assertEqual({ok, "aaa", ~"bbb"}, Parser(~"aaabbb")),
    %% Match zero characters (should succeed with empty list)
    ?assertEqual({ok, [], ~"bbb"}, Parser(~"bbb")),
    %% Match empty input
    ?assertEqual({ok, [], ~""}, Parser(~"")).


many_digit_test() ->
    %% Match multiple digits and convert to integer
    Parser = map(many(digit()), fun(Ds) -> list_to_integer(Ds) end),
    ?assertEqual({ok, 123, ~"abc"}, Parser(~"123abc")),
    %% If no digits, list_to_integer would fail, so many should be followed by validation or use many1
    ?assertEqual({ok, [], ~"abc"}, (many(digit()))(~"abc")).


combination_test() ->
    %% Parse "a" then zero or more "b"s
    Parser = sequence(char($a), many(char($b))),
    ?assertEqual({ok, {$a, "bbb"}, ~"c"}, Parser(~"abbbc")),
    ?assertEqual({ok, {$a, []}, ~"c"}, Parser(~"ac")).


lazy_test() ->
    %% A simple recursive parser matching arbitrarily nested parentheses: () or (()) or ((()))
    %% Since Erlang variables cannot be directly recursive without being passed as arguments or using an ETS/module lookup,
    %% we wrap the recursive call in a function for `lazy`.
    Parens = fun F() ->
                     epc:choice(
                       %% Fix: pass binary to string/1
                       epc:string(~"()"),
                       epc:map(
                         epc:sequence(epc:char($(), epc:sequence(epc:lazy(F), epc:char($)))),
                         fun({_, {Inner, _}}) -> ["(", Inner, ")"] end))
             end,
    Parser = Parens(),
    ?assertEqual({ok, ~"()", ~""}, epc:parse(Parser, ~"()")),
    ?assertEqual({ok, ["(", ~"()", ")"], ~""}, epc:parse(Parser, ~"(())")).


sep_by_test() ->
    %% Parse comma-separated digits
    P = sep_by(digit(), char($,)),
    %% Match multiple
    ?assertEqual({ok, [$1, $2, $3], ~""}, epc:parse(P, ~"1,2,3")),
    %% Match one
    ?assertEqual({ok, [$1], ~""}, epc:parse(P, ~"1")),
    %% Match none
    ?assertEqual({ok, [], ~"abc"}, epc:parse(P, ~"abc")).


satisfy_and_string_test() ->
    %% Parse a simple string like "hello" (ignoring escapes for simplicity)
    StringChar = none_of("\""),
    JsonStringParser = map(
                         sequence(char($"), sequence(many(StringChar), char($"))),
                         fun({_, {Str, _}}) -> list_to_binary(Str) end),
    %% Fix: many(StringChar) returns a list, expect "hello" and "world"
    ?assertEqual({ok, ~"hello", ~""}, epc:parse(JsonStringParser, ~"\"hello\"")),
    ?assertEqual({ok, ~"world", ~" followed"}, epc:parse(JsonStringParser, ~"\"world\" followed")).


spaces_test() ->
    %% Parse a token ignoring trailing spaces
    TokenP = token(string(~"true")),
    ?assertEqual({ok, ~"true", ~"false"}, epc:parse(TokenP, ~"true  \n \t false")).


optional_test() ->
    %% Parse an optional minus sign followed by digits
    MinusP = optional(char($-)),
    NumP = map(
             sequence(MinusP, many1(digit())),
             fun({Minus, Ds}) ->
                     Int = list_to_integer(Ds),
                     case Minus of
                         undefined -> Int;
                         $- -> -Int
                     end
             end),
    ?assertEqual({ok, 123, ~""}, epc:parse(NumP, ~"123")),
    ?assertEqual({ok, -45, ~""}, epc:parse(NumP, ~"-45")).


-endif.
