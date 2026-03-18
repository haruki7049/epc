# epc

Erlang Parser Combinator

## Overview

A parser combinator library implemented in Erlang. It supports parsing everything from basic characters and strings to complex structures like JSON and S-expressions.

## Usage Example

```erlang
%% Define key parser
KeyParser = epc:string(<<"id">>),
%% Define colon parser
ColonParser = epc:char($:),
%% Parse digits and map to integer
ValueParser = epc:map(epc:many(epc:digit()), fun(Ds) -> list_to_integer(Ds) end),

%% Combine parsers into a sequence
KeyValueParser = epc:sequence(KeyParser, epc:sequence(ColonParser, ValueParser)),

%% Execute parsing
epc:parse(KeyValueParser, <<"id:1024">>).
```
