[![Hex.pm Version](https://img.shields.io/hexpm/v/erl_csv.svg)](https://hex.pm/packages/erl_csv)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/erl_csv/)
[![Hex Downloads](https://img.shields.io/hexpm/dt/erl_csv.svg)](https://hex.pm/packages/erl_csv)
[![Erlang/OTP Versions](https://img.shields.io/badge/erlang%2Fotp-27%7C28%7C29-blue)](https://www.erlang.org)
[![Build Status](https://github.com/NelsonVides/erl_csv/actions/workflows/erlang.yml/badge.svg?branch=main)](https://github.com/NelsonVides/erl_csv/actions/workflows/erlang.yml)

`erl_csv` is a pure Erlang library to encode and decode csv files.

## Examples:

### Encoding
```erlang

    DataSchema = [<<"city">>, <<"country">>, <<"continent">>],
    DataRows = [
          [<<"Madrid">>, <<"Spain">>, <<"Europe">>],
          [<<"Krakow">>, <<"Poland">>, <<"Europe">>],
          [<<"Berlin">>, <<"Germany">>, <<"Europe">>],
          [<<"New York">>, <<"USA">>, <<"America">>],
          [<<"Sidney">>, <<"Australia">>, <<"Asia">>]
    ],
    Encoded = erl_csv:encode([DataSchema | DataRows]),
```

Then, in a single readable binary as `iolist_to_binary(Encoded)`, we will see:
```erlang
    <<"city,country,continent\nMadrid,Spain,Europe\nKrakow,Poland,Europe\nBerlin,Germany,Europe\nNew York,USA,America\nSidney,Au"...>>
```

### Decoding
If we are reading a file, we can decode as a stream as follows:
```erlang
    {ok, CsvStream} = erl_csv:decode_new_s(Filename),
    {ok, do_import(CsvStream, WorkersQueue)};

do_import(stream_end, Decoded) ->
    lists:reverse(Decoded);
do_import(Stream, Decoded) ->
    {ok, MoreDecoded, MoreStream} = erl_csv:decode_s(Stream),
    do_import(MoreStream, [MoreDecoded | Decoded]).

```

## More details
[See the documentation for more details.](https://hexdocs.pm/erl_csv/)
