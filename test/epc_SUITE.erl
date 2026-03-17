-module(epc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([parse_key_value_test/1, choice_and_many_test/1]).


all() ->
    [parse_key_value_test, choice_and_many_test].


%% Integration test: Parse "key:value" format
parse_key_value_test(_Config) ->
    %% string/1 now expects a binary
    KeyParser = epc:string(~"id"),
    ColonParser = epc:char($:),
    ValueParser = epc:map(epc:many(epc:digit()), fun(Ds) -> list_to_integer(Ds) end),

    %% Combine sequence: "id", ":", and digits
    KeyValueParser = epc:sequence(KeyParser, epc:sequence(ColonParser, ValueParser)),

    %% Assert success case (input and remaining string must be binary)
    ExpectedSuccess = {ok, {~"id", {$:, 1024}}, ~""},
    ?assertEqual(ExpectedSuccess, epc:parse(KeyValueParser, ~"id:1024")),

    %% Assert failure case (invalid key)
    %% Note: Error message remains a string list in epc:string/1
    ExpectedError = {error, "Expected string"},
    ?assertEqual(ExpectedError, epc:parse(KeyValueParser, ~"name:1024")),
    ok.


%% Integration test: Parse choice followed by many characters
choice_and_many_test(_Config) ->
    PrefixParser = epc:choice(epc:char($A), epc:char($B)),
    SuffixParser = epc:many(epc:char($x)),

    %% Combine prefix (A or B) and suffix (zero or more 'x')
    Parser = epc:sequence(PrefixParser, SuffixParser),

    %% Assert choice A
    %% many() returns a list, so "xxx" is correct, but the rest is binary ~"y"
    ExpectedA = {ok, {$A, "xxx"}, ~"y"},
    ?assertEqual(ExpectedA, epc:parse(Parser, ~"Axxxy")),

    %% Assert choice B
    ExpectedB = {ok, {$B, []}, ~"y"},
    ?assertEqual(ExpectedB, epc:parse(Parser, ~"By")),
    ok.
