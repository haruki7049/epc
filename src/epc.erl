-module(epc).

%% Types definition
-type parse_result(T) :: {ok, T, string()} | {error, string()}.
-type parser(T) :: fun((string()) -> parse_result(T)).

-export([char/1, sequence/2, choice/2, parse/2, string/1, digit/0, map/2, many/1]).


%% @doc Parse a specific character
-spec char(char()) -> parser(char()).
char(Target) ->
    fun([H | T]) when H =:= Target -> {ok, H, T};
       (_) -> {error, "Unexpected character"}
    end.


%% @doc Parse a specific string
-spec string(string()) -> parser(string()).
string([]) -> fun(Input) -> {ok, "", Input} end;
string([H | T]) ->
    map(sequence(char(H), string(T)), fun({C, S}) -> [C | S] end).


%% @doc Parse a digit character
-spec digit() -> parser(char()).
digit() ->
    fun([H | T]) when H >= $0, H =< $9 -> {ok, H, T};
       (_) -> {error, "Expected digit"}
    end.


%% @doc Transform the result of a parser
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


%% @doc Zero or more repetitions
-spec many(parser(T)) -> parser([T]).
many(P) ->
    fun(Input) ->
            case P(Input) of
                {ok, R, Rest} ->
                    {ok, Rs, FinalRest} = (many(P))(Rest),
                    {ok, [R | Rs], FinalRest};
                {error, _} ->
                    {ok, [], Input}
            end
    end.


%% @doc Combine two parsers to run in sequence
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


%% @doc Try the first parser, if it fails, try the second
-spec choice(parser(T), parser(T)) -> parser(T).
choice(P1, P2) ->
    fun(Input) ->
            case P1(Input) of
                {ok, R, Rest} -> {ok, R, Rest};
                {error, _} -> P2(Input)
            end
    end.


%% @doc Helper to run a parser
-spec parse(parser(T), string()) -> parse_result(T).
parse(Parser, Input) ->
    Parser(Input).


%% Include EUnit header
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


char_test() ->
    Parser = char($a),
    %% Success case
    ?assertEqual({ok, $a, "bc"}, Parser("abc")),
    %% Failure case
    ?assertEqual({error, "Unexpected character"}, Parser("bbc")).


sequence_test() ->
    P = sequence(char($a), char($b)),
    %% Success case
    ?assertEqual({ok, {$a, $b}, "c"}, P("abc")),
    %% Failure case (first fails)
    ?assertEqual({error, "Unexpected character"}, P("bbc")),
    %% Failure case (second fails)
    ?assertEqual({error, "Unexpected character"}, P("acb")).


choice_test() ->
    P = choice(char($a), char($b)),
    %% Match first
    ?assertEqual({ok, $a, "c"}, P("ac")),
    %% Match second
    ?assertEqual({ok, $b, "c"}, P("bc")),
    %% Match neither
    ?assertEqual({error, "Unexpected character"}, P("cc")).


digit_to_int_test() ->
    %% Combine digit, many, and map to parse an integer
    IntParser = map(many(digit()), fun(Ds) -> list_to_integer(Ds) end),
    ?assertEqual({ok, 123, "abc"}, IntParser("123abc")).


string_test() ->
    P = string("hello"),
    ?assertEqual({ok, "hello", " world"}, P("hello world")).


-endif.
