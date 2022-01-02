-module(erl_csv).

-export([encode/1, encode/2, decode/1, decode/2]).

-type encode_opts() :: #{headers => boolean() | list(),
                         separator => <<_:8>>,
                         delimiter => binary()}.

-type decode_opts() :: #{quotes => <<_:8>>,
                         separator => <<_:8>>,
                         delimiter => binary()}.

-export_type([encode_opts/0, decode_opts/0]).

% @doc """
% Encode a table stream into a stream of RFC 4180 compliant CSV lines for
% writing to a file or other IO.
% ## Options
% These are the options:
%   * `:separator`   – The separator token to use, defaults to `?,`.
%     Must be a codepoint (syntax: ? + your separator token).
%   * `:delimiter`   – The delimiter token to use, defaults to `\"\\r\\n\"`.
%   * `:headers`     – When set to `true`, uses the keys of the first map as
%     the first element in the stream. All subsequent elements are the values
%     of the maps. When set to a list, will use the given list as the first
%     element in the stream and order all subsequent elements using that list.
%     When set to `false` (default), will use the raw inputs as elements.
%     When set to anything but `false`, all elements in the input stream are
%     assumed to be maps.

-spec encode(iolist() | list(map())) -> iolist().
encode(Input) ->
    encode(Input, #{}).

-spec encode(iolist() | list(map()), encode_opts()) -> iolist().
encode(Input, Opts) ->
    erl_csv_encoder:encode(Input, Opts).

% @doc """
% Decode a stream of comma-separated lines into a stream of rows.
% You can control the number of parallel work streams via the option
% `:num_workers` - default is the number of erlang schedulers times 3.
% The Decoder expects line by line input of valid csv lines with inlined
% escape sequences if you use it directly.

% ## Options

% These are the options:

% * `:separator`   – The separator token to use, defaults to `?,`.
%     Must be a codepoint (syntax: ? + (your separator)).
% * `:strip_fields` – When set to true, will strip whitespace from fields.
%     Defaults to false.
% * `:num_workers` – The number of parallel operations to run when producing
%     the stream.
% * `:worker_work_ratio` – The available work per worker, defaults to 5.
%     Higher rates will mean more work sharing, but might also lead to work
%     fragmentation slowing down the queues.
% * `:headers`     – When set to `true`, will take the first row of the csv
%     and use it as header values.
%     When set to a list, will use the given list as header values.
%     When set to `false` (default), will use no header values.
%     When set to anything but `false`, the resulting rows in the matrix will
%     be maps instead of lists.
% * `:replacement`   – The replacement string to use where lines have bad
%     encoding. Defaults to `nil`, which disables replacement.

-spec decode(iolist() | list(map())) -> {ok, iolist()} | {error, iolist()}.
decode(Input) ->
    decode(Input, #{}).

-spec decode(iolist() | list(map()), decode_opts()) -> {ok, iolist()} | {error, iolist()}.
decode(Input, Opts) ->
    erl_csv_decoder:decode(Input, Opts).
