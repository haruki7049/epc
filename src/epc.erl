-module(epc).

%% Types definition
-type parse_result(T) :: {ok, T, string()} | {error, string()}.
-type parser(T) :: fun((string()) -> parse_result(T)).

-export([char/1, sequence/2, choice/2, parse/2]).


%% @doc Parse a specific character
-spec char(char()) -> parser(char()).
char(Target) ->
    fun([H | T]) when H =:= Target -> {ok, H, T};
       (_) -> {error, "Unexpected character"}
    end.


%% @doc Combine two parsers to run in sequence
-spec sequence(parser(T), parser(U)) -> parser({T, U}).
sequence(P1, P2) ->
    fun(Input) ->
            case P1(Input) of
                {ok, R1, Rest1} ->
                    case P2(Rest1) of
                        {ok, R2, Rest2} -> {ok, {R1, R2}, Rest2};
                        {error, Reason} -> {error, Reason}
                    end;
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


-endif.
