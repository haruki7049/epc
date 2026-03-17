-module(epc_json_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([parse_json_test/1]).


all() ->
    [parse_json_test].


%% Internal helper to define the JSON parser
json_parser() ->
    %% Punctuation tokens ignoring trailing spaces
    LBrace = epc:token(epc:char(${)),
    RBrace = epc:token(epc:char($})),
    LBracket = epc:token(epc:char($[)),
    RBracket = epc:token(epc:char($])),
    Comma = epc:token(epc:char($,)),
    Colon = epc:token(epc:char($:)),

    %% Simple value parsers
    NullP = epc:map(epc:token(epc:string("null")), fun(_) -> null end),
    TrueP = epc:map(epc:token(epc:string("true")), fun(_) -> true end),
    FalseP = epc:map(epc:token(epc:string("false")), fun(_) -> false end),

    %% Number parser (integers with optional minus)
    MinusP = epc:optional(epc:char($-)),
    DigitsP = epc:many1(epc:digit()),
    NumberP = epc:token(
                epc:map(
                  epc:sequence(MinusP, DigitsP),
                  fun({Minus, Ds}) ->
                          Int = list_to_integer(Ds),
                          case Minus of
                              undefined -> Int;
                              $- -> -Int
                          end
                  end)),

    %% String parser (simplified, no escape sequences)
    Quote = epc:char($"),
    StringContent = epc:many(epc:none_of("\"")),
    StringP = epc:token(
                epc:map(
                  epc:sequence(Quote, epc:sequence(StringContent, Quote)),
                  fun({_, {Str, _}}) -> list_to_binary(Str) end  %% Convert to binary for JSON strings
                  )),

    %% Recursive value parser using lazy
    ValueP = fun F() ->
                     epc:choice(
                       NullP,
                       epc:choice(
                         TrueP,
                         epc:choice(
                           FalseP,
                           epc:choice(
                             NumberP,
                             epc:choice(
                               StringP,
                               epc:choice(
                                 %% Array parser
                                 epc:map(
                                   epc:sequence(LBracket, epc:sequence(epc:sep_by(epc:lazy(F), Comma), RBracket)),
                                   fun({_, {Elements, _}}) -> Elements end),
                                 %% Object parser
                                 epc:map(
                                   epc:sequence(
                                     LBrace,
                                     epc:sequence(
                                       epc:sep_by(
                                         epc:map(
                                           epc:sequence(StringP, epc:sequence(Colon, epc:lazy(F))),
                                           fun({Key, {_, Val}}) -> {Key, Val} end),
                                         Comma),
                                       RBrace)),
                                   fun({_, {Pairs, _}}) -> maps:from_list(Pairs) end)))))))
             end,

    %% The main parser allows leading spaces before the JSON value
    epc:map(
      epc:sequence(epc:spaces(), ValueP()),
      fun({_, FinalValue}) -> FinalValue end).


%% Integration test: Parse a complex JSON string
parse_json_test(_Config) ->
    Parser = json_parser(),

    JsonString = "
    {
        \"id\": 101,
        \"name\": \"epc\",
        \"is_active\": true,
        \"tags\": [\"parser\", \"combinator\", null],
        \"metadata\": {
            \"version\": -1
        }
    }",

    Expected = #{
                 ~"id" => 101,
                 ~"name" => ~"epc",
                 ~"is_active" => true,
                 ~"tags" => [~"parser", ~"combinator", null],
                 ~"metadata" => #{
                                  ~"version" => -1
                                 }
                },

    ?assertEqual({ok, Expected, ""}, epc:parse(Parser, JsonString)),
    ok.
