-module(erl_csv).
-moduledoc """
A simple library for encoding and decoding RFC 4180 compliant CSV.

It can encode Erlang terms into CSV `t:iolist/0` and decode CSV binaries back
into rows of binaries, either all at once or lazily from a file stream.

All input and output is treated as UTF-8: multi-byte codepoints are preserved
verbatim on both the encoding and the decoding path.
""".

-include("erl_csv.hrl").

-export([encode/1, encode/2, decode/1, decode/2]).
-export([decode_new_s/1, decode_new_s/2, decode_s/1]).

-doc "Options accepted by `encode/2`.".
-type encode_opts() :: #{headers => boolean() | list(),
                         separator => <<_:8>>,
                         delimiter => binary()}.

-doc "Options accepted by `decode/2` and `decode_new_s/2`.".
-type decode_opts() :: #{quotes => <<_:8>>,
                         separator => <<_:8>>,
                         delimiter => binary(),
                         regex => term()}.

-type csv_stream() :: #csv_stream{} | stream_end.
-type maybe_csv_stream() :: csv_stream() | {error, term()}.
-type csv_stream_fun() :: fun(() -> maybe_csv_stream()).

-export_type([csv_stream/0, maybe_csv_stream/0, csv_stream_fun/0]).
-export_type([encode_opts/0, decode_opts/0]).

-doc #{equiv => encode/2}.
-spec encode(iolist() | list(map())) -> iolist().
encode(Input) ->
    encode(Input, #{}).

-doc """
Encode a table into a stream of RFC 4180 compliant CSV lines for writing to a
file or other IO.

## Options

- `separator` — The separator token to use, defaults to `<<",">>`. Must be a
  single byte.
- `delimiter` — The line delimiter token to use, defaults to `<<"\\n">>`.
- `headers` — When set to `true`, uses the keys of the first map as the first
  element in the stream. All subsequent elements are the values of the maps.
  When set to a list, uses the given list as the first element in the stream and
  orders all subsequent elements using that list. When set to `false` (the
  default), uses the raw inputs as elements. When set to anything but `false`,
  all elements in the input are assumed to be maps.
""".
-spec encode(iolist() | list(map()), encode_opts()) -> iolist().
encode(Input, Opts) ->
    erl_csv_encoder:encode(Input, Opts).

-doc #{equiv => decode/2}.
-spec decode(iodata()) ->
    {ok, iodata()} | {has_trailer, iodata(), iodata()} | {nomatch, iodata()} | {error, term()}.
decode(Input) ->
    decode(Input, #{}).

-doc """
Decode a chunk of comma-separated lines into rows.

The decoder expects line by line input of valid CSV lines with inlined escape
sequences if you use it directly. When the chunk ends in the middle of a line,
`{has_trailer, Decoded, Trailer}` is returned so the caller can prepend
`Trailer` to the next chunk.

## Options

- `separator` — The separator token to use, defaults to `<<",">>`. Must be a
  single byte.
- `delimiter` — The line delimiter token to use, defaults to `<<"\\n">>`. May be
  more than one byte, e.g. `<<"\\r\\n">>`.
- `quotes` — The quoting token to use, defaults to `<<"\\"">>`. Must be a single
  byte.
- `headers` — When set to `true`, takes the first row of the CSV and uses it as
  header values. When set to a list, uses the given list as header values. When
  set to `false` (the default), uses no header values. When set to anything but
  `false`, the resulting rows are maps instead of lists.
""".
-spec decode(iodata(), decode_opts()) ->
    {ok, iodata()} | {has_trailer, iodata(), iodata()} | {nomatch, iodata()} | {error, term()}.
decode(Input, Opts) ->
    erl_csv_decoder:decode(Input, Opts).

-doc #{equiv => decode_new_s/2}.
-spec decode_new_s(file:name_all()) ->
    {ok, csv_stream()} | {error, term()}.
decode_new_s(Input) ->
    decode_new_s(Input, #{}).

-doc """
Open a file and return a lazy `t:csv_stream/0` to be consumed with `decode_s/1`.

Accepts the same options as `decode/2`.
""".
-spec decode_new_s(file:name_all(), decode_opts()) ->
    {ok, csv_stream()} | {error, term()}.
decode_new_s(Input, Opts) ->
    erl_csv_decoder:decode_new_s(Input, Opts).

-doc """
Decode the next rows out of a `t:csv_stream/0` returned by `decode_new_s/1,2`.

Returns the decoded rows together with the continuation stream. When the stream
is exhausted, the returned rows are `[]` and the stream is `stream_end`.
""".
-spec decode_s(csv_stream()) ->
    {ok, iolist(), csv_stream()} | {error, term()}.
decode_s(Input) ->
    erl_csv_decoder:decode_s(Input).
