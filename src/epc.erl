-module(epc).
-export([echo/0]).


-spec echo() -> ok.
echo() ->
    io:format("hoge~n").
