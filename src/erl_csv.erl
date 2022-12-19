-module(erl_csv).

-include("erl_csv.hrl").

-export([encode/1, encode/2, decode/1, decode/2]).
-export([decode_new_s/1, decode_new_s/2, decode_s/1]).

-type encode_opts() :: #{headers => boolean() | list(),
                         separator => <<_:8>>,
                         delimiter => binary()}.

-type decode_opts() :: #{quotes => <<_:8>>,
                         separator => <<_:8>>,
                         delimiter => binary(),
                         regex => term()}.

-export_type([encode_opts/0, decode_opts/0]).

% @doc
% Encode a table stream into a stream of RFC 4180 compliant CSV lines for
% writing to a file or other IO.
%
% Options
%
%   * `:separator'   – The separator token to use, defaults to `?,'.
%     Must be a codepoint (syntax: ? + your separator token).
%
%   * `:delimiter'   – The delimiter token to use, defaults to `\"\\r\\n\"'.
%
%   * `:headers'     – When set to `true', uses the keys of the first map as
%     the first element in the stream. All subsequent elements are the values
%     of the maps. When set to a list, will use the given list as the first
%     element in the stream and order all subsequent elements using that list.
%     When set to `false' (default), will use the raw inputs as elements.
%     When set to anything but `false', all elements in the input stream are
%     assumed to be maps.

-spec encode(iolist() | list(map())) -> iolist().
encode(Input) ->
    encode(Input, #{}).

-spec encode(iolist() | list(map()), encode_opts()) -> iolist().
encode(Input, Opts) ->
    erl_csv_encoder:encode(Input, Opts).

% @doc
% Decode a stream of comma-separated lines into a stream of rows.
% The Decoder expects line by line input of valid csv lines with inlined
% escape sequences if you use it directly.
%
% Options
%
% * `:separator'   – The separator token to use, defaults to `?,'.
%     Must be a codepoint (syntax: ? + (your separator)).
%
% * `:headers'     – When set to `true', will take the first row of the csv
%     and use it as header values.
%     When set to a list, will use the given list as header values.
%     When set to `false' (default), will use no header values.
%     When set to anything but `false', the resulting rows in the matrix will
%     be maps instead of lists.

-spec decode(iodata()) ->
    {ok, iodata()} | {has_trailer, iodata(), iodata()} | {nomatch, iodata()} | {error, term()}.
decode(Input) ->
    decode(Input, #{}).

-spec decode(iodata(), decode_opts()) ->
    {ok, iodata()} | {has_trailer, iodata(), iodata()} | {nomatch, iodata()} | {error, term()}.
decode(Input, Opts) ->
    erl_csv_decoder:decode(Input, Opts).

-spec decode_new_s(file:name_all()) ->
    {ok, csv_stream()} | {error, term()}.
decode_new_s(Input) ->
    decode_new_s(Input, #{}).

-spec decode_new_s(file:name_all(), decode_opts()) ->
    {ok, csv_stream()} | {error, term()}.
decode_new_s(Input, Opts) ->
    erl_csv_decoder:decode_new_s(Input, Opts).

-spec decode_s(csv_stream()) ->
    {ok, iolist(), csv_stream()} | {error, term()}.
decode_s(Input) ->
    erl_csv_decoder:decode_s(Input).
