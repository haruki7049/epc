-module(epc_sexp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([parse_sexp_test/1]).


all() ->
    [parse_sexp_test].


%% Internal helper to define the S-expression parser
sexp_parser() ->
    %% Punctuation tokens ignoring trailing spaces
    LParen = epc:token(epc:char($()),
    RParen = epc:token(epc:char($))),

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
                  fun({_, {Str, _}}) -> list_to_binary(Str) end)),

    %% Symbol parser (identifiers, operators, etc.)
    IsSymChar = fun(C) ->
                        (C >= $a andalso C =< $z) orelse
                        (C >= $A andalso C =< $Z) orelse
                        (C >= $0 andalso C =< $9) orelse
                        lists:member(C, "+-*/_=")
                end,
    SymbolP = epc:token(
                epc:map(
                  epc:many1(epc:satisfy(IsSymChar)),
                  %% Tag symbols to differentiate them from strings
                  fun(Chars) -> {symbol, list_to_binary(Chars)} end)),

    %% Recursive S-expression parser using lazy and choice/1
    SExpP = fun F() ->
                    epc:choice([NumberP,
                                StringP,
                                SymbolP,
                                %% List parser: ( expr1 expr2 ... )
                                epc:map(
                                  epc:sequence(LParen, epc:sequence(epc:many(epc:lazy(F)), RParen)),
                                  fun({_, {Elements, _}}) -> Elements end)])
            end,

    %% The main parser allows leading spaces before the S-expression
    epc:map(
      epc:sequence(epc:spaces(), SExpP()),
      fun({_, FinalValue}) -> FinalValue end).


%% Integration test: Parse a complex S-expression
parse_sexp_test(_Config) ->
    Parser = sexp_parser(),

    %% Binary input containing a recursive function definition
    Input = ~"
        (defun factorial (n)
            (if (= n 0)
                1
                (* n (factorial (- n 1)))))
    ",

    %% Expected nested list structure
    Expected = [{symbol, ~"defun"},
                {symbol, ~"factorial"},
                [{symbol, ~"n"}],
                [{symbol, ~"if"},
                 [{symbol, ~"="}, {symbol, ~"n"}, 0],
                 1,
                 [{symbol, ~"*"},
                  {symbol, ~"n"},
                  [{symbol, ~"factorial"}, [{symbol, ~"-"}, {symbol, ~"n"}, 1]]]]],

    ?assertEqual({ok, Expected, ~""}, epc:parse(Parser, Input)),
    ok.
