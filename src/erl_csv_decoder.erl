-module(erl_csv_decoder).
-moduledoc false.

-include("erl_csv.hrl").

-export([decode/2, decode_new_s/2, decode_s/1]).

%% The decoder is a single tail-recursive loop that walks the chunk one byte at a time.
%% Check with `erlc +bin_opt_info' that every loop function keeps reusing the context.
-record(csv_decoder, {
    separator :: byte(),
    quote :: byte(),
    %% First byte of the line delimiter, and the bytes that must follow it.
    delimiter :: byte(),
    delimiter_rest :: binary()
}).
-type csv_decoder() :: #csv_decoder{}.

-type row() :: [binary()].
-type decoded() :: {ok, [row()]} | {has_trailer, [row()], binary()} | {nomatch, binary()}.
%% Offsets of the quotes to drop from a quoted field, used in LIFO order
-type escapes() :: [non_neg_integer()].

-spec decode(iodata(), erl_csv:decode_opts()) ->
    {ok, iodata()} | {has_trailer, iodata(), iodata()} | {nomatch, iodata()}.
decode(Chunk, Opts) ->
    Bin = iolist_to_binary(Chunk),
    <<Separator>> = maps:get(separator, Opts, ?SEPARATOR),
    <<Quote>> = maps:get(quotes, Opts, ?QUOTES),
    <<Delimiter, DelimiterRest/binary>> = maps:get(delimiter, Opts, ?DELIMITER),
    State = #csv_decoder{
        separator = Separator,
        quote = Quote,
        delimiter = Delimiter,
        delimiter_rest = DelimiterRest
    },
    field_start(Bin, Bin, 0, 0, [], [], State).

-spec decode_new_s(file:name_all(), erl_csv:decode_opts()) ->
    {ok, erl_csv:csv_stream()} | {error, term()}.
decode_new_s(File, Opts) ->
    case erl_csv_file_stream:read_file(File, Opts) of
        #csv_stream{} = Stream ->
            {ok, Stream};
        {error, Reason} ->
            {error, Reason}
    end.

-spec decode_s(erl_csv:maybe_csv_stream()) ->
    {ok, iodata(), erl_csv:csv_stream()} | {error, term()}.
decode_s(stream_end) ->
    {ok, [], stream_end};
decode_s(#csv_stream{hd = Bin, opts = Opts} = Stream) ->
    case decode(Bin, Opts) of
        {ok, Decoded} ->
            {ok, Decoded, Stream#csv_stream{hd = <<>>}};
        {has_trailer, Decoded, Trailer} ->
            SavedStream = Stream#csv_stream{hd = Trailer},
            case get_more_stream(SavedStream) of
                {error, Reason} -> {error, Reason};
                stream_end -> last_rows(Decoded, Trailer, Opts);
                MoreStream -> {ok, Decoded, MoreStream}
            end;
        {nomatch, NotMatched} ->
            SavedStream = Stream#csv_stream{hd = NotMatched},
            case get_more_stream(SavedStream) of
                stream_end -> last_rows([], NotMatched, Opts);
                MoreStream -> decode_s(MoreStream)
            end
    end;
decode_s({error, Reason}) ->
    {error, Reason}.

-spec last_rows([[binary()]], binary(), erl_csv:decode_opts()) ->
    {ok, [[binary()]], erl_csv:csv_stream()} | {error, term()}.
last_rows(Decoded, <<>>, _Opts) ->
    {ok, Decoded, stream_end};
last_rows(Decoded, Trailer, Opts) ->
    Delimiter = maps:get(delimiter, Opts, ?DELIMITER),
    case decode(<<Trailer/binary, Delimiter/binary>>, Opts) of
        {ok, Rows} ->
            {ok, Decoded ++ Rows, stream_end};
        _ when Decoded =:= [] ->
            {error, {unterminated_quoted_field, Trailer}};
        _ ->
            {ok, Decoded, #csv_stream{hd = Trailer, tl = fun() -> stream_end end, opts = Opts}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_more_stream(erl_csv:csv_stream()) -> erl_csv:maybe_csv_stream().
get_more_stream(Stream) ->
    case erl_csv_file_stream:tl(Stream) of
        {error, Reason} ->
            {error, Reason};
        NewStream ->
            NewStream
    end.

%% At the first byte of a field, at offset Pos. The fields of the current row so
%% far are in Row (reversed); the row started at offset RowStart, which is where
%% the trailer begins if the chunk ends before the row does.
-spec field_start(
    binary(), binary(), non_neg_integer(), non_neg_integer(), row(), [row()], csv_decoder()
) -> decoded().
field_start(<<C, Rest/binary>>, Bin, Pos, RowStart, Row, Rows, State) ->
    case State of
        #csv_decoder{quote = C} ->
            quoted(Rest, Bin, Pos + 1, 0, RowStart, Row, Rows, [], State);
        #csv_decoder{separator = C} ->
            field_start(Rest, Bin, Pos + 1, RowStart, [<<>> | Row], Rows, State);
        #csv_decoder{delimiter = C, delimiter_rest = DelimiterRest} ->
            N = byte_size(DelimiterRest),
            case Rest of
                <<DelimiterRest:N/binary, Rest1/binary>> ->
                    Next = Pos + 1 + N,
                    Fields = lists:reverse(Row, [<<>>]),
                    field_start(Rest1, Bin, Next, Next, [], [Fields | Rows], State);
                _ ->
                    unquoted(Rest, Bin, Pos, 1, RowStart, Row, Rows, State)
            end;
        _ ->
            unquoted(Rest, Bin, Pos, 1, RowStart, Row, Rows, State)
    end;
field_start(<<>>, Bin, _Pos, _RowStart, [], [], _State) ->
    {nomatch, Bin};
field_start(<<>>, _Bin, _Pos, _RowStart, [], Rows, _State) ->
    {ok, lists:reverse(Rows)};
field_start(<<>>, Bin, _Pos, RowStart, _Row, Rows, State) ->
    %% A separator was the last byte of the chunk: its row is incomplete.
    trailer(Bin, RowStart, Rows, State).

%% Inside an unquoted field that started at Start and is Len bytes long so far.
%% Quotes are not special here: a stray quote is kept verbatim.
-spec unquoted(
    binary(),
    binary(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    row(),
    [row()],
    csv_decoder()
) -> decoded().
unquoted(<<C, Rest/binary>>, Bin, Start, Len, RowStart, Row, Rows, State) ->
    case State of
        #csv_decoder{separator = C} ->
            Field = binary_part(Bin, Start, Len),
            field_start(Rest, Bin, Start + Len + 1, RowStart, [Field | Row], Rows, State);
        #csv_decoder{delimiter = C, delimiter_rest = DelimiterRest} ->
            N = byte_size(DelimiterRest),
            case Rest of
                <<DelimiterRest:N/binary, Rest1/binary>> ->
                    Next = Start + Len + 1 + N,
                    Fields = lists:reverse(Row, [binary_part(Bin, Start, Len)]),
                    field_start(Rest1, Bin, Next, Next, [], [Fields | Rows], State);
                _ ->
                    %% Only a prefix of a multi-byte delimiter: part of the field.
                    unquoted(Rest, Bin, Start, Len + 1, RowStart, Row, Rows, State)
            end;
        _ ->
            unquoted(Rest, Bin, Start, Len + 1, RowStart, Row, Rows, State)
    end;
unquoted(<<>>, Bin, _Start, _Len, RowStart, _Row, Rows, State) ->
    trailer(Bin, RowStart, Rows, State).

%% Inside a quoted field whose content started at Start and is Len bytes long so
%% far (counting both quotes of any doubled pair seen, recorded in Escapes).
-spec quoted(
    binary(),
    binary(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    row(),
    [row()],
    escapes(),
    csv_decoder()
) -> decoded().
quoted(<<C, Rest/binary>>, Bin, Start, Len, RowStart, Row, Rows, Escapes, State) ->
    case State of
        #csv_decoder{quote = C} ->
            after_quote(Rest, Bin, Start, Len, RowStart, Row, Rows, Escapes, State);
        _ ->
            quoted(Rest, Bin, Start, Len + 1, RowStart, Row, Rows, Escapes, State)
    end;
quoted(<<>>, Bin, _Start, _Len, RowStart, _Row, Rows, _Escapes, State) ->
    trailer(Bin, RowStart, Rows, State).

%% Right after a quote inside a quoted field: either the first quote of a
%% doubled (escaped) pair, or the closing quote, which must be followed by a
%% separator or a delimiter.
-spec after_quote(
    binary(),
    binary(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    row(),
    [row()],
    escapes(),
    csv_decoder()
) -> decoded().
after_quote(<<C, Rest/binary>>, Bin, Start, Len, RowStart, Row, Rows, Escapes, State) ->
    case State of
        #csv_decoder{quote = C} ->
            Escapes1 = [Start + Len + 1 | Escapes],
            quoted(Rest, Bin, Start, Len + 2, RowStart, Row, Rows, Escapes1, State);
        #csv_decoder{separator = C} ->
            Field = quoted_value(Bin, Start, Len, Escapes),
            field_start(Rest, Bin, Start + Len + 2, RowStart, [Field | Row], Rows, State);
        #csv_decoder{delimiter = C, delimiter_rest = DelimiterRest} ->
            N = byte_size(DelimiterRest),
            case Rest of
                <<DelimiterRest:N/binary, Rest1/binary>> ->
                    Next = Start + Len + 2 + N,
                    Fields = lists:reverse(Row, [quoted_value(Bin, Start, Len, Escapes)]),
                    field_start(Rest1, Bin, Next, Next, [], [Fields | Rows], State);
                _ ->
                    malformed(Bin, Start, RowStart, Row, Rows, State)
            end;
        _ ->
            malformed(Bin, Start, RowStart, Row, Rows, State)
    end;
after_quote(<<>>, Bin, _Start, _Len, RowStart, _Row, Rows, _Escapes, State) ->
    %% Closing quote but no terminator yet: the row is not complete.
    trailer(Bin, RowStart, Rows, State).

%% Content between the closing quote and the terminator is malformed; fall back
%% to reading the whole field (from the opening quote) verbatim rather than
%% dropping data.
-spec malformed(binary(), non_neg_integer(), non_neg_integer(), row(), [row()], csv_decoder()) ->
    decoded().
malformed(Bin, Start, RowStart, Row, Rows, State) ->
    OpeningQuote = Start - 1,
    <<_:OpeningQuote/binary, Rest/binary>> = Bin,
    unquoted(Rest, Bin, OpeningQuote, 0, RowStart, Row, Rows, State).

%% Only pay for un-escaping (a copy) when a doubled quote was actually seen;
%% otherwise the value is the raw slice as-is.
-spec quoted_value(binary(), non_neg_integer(), non_neg_integer(), escapes()) -> binary().
quoted_value(Bin, Start, Len, []) ->
    binary_part(Bin, Start, Len);
quoted_value(Bin, Start, Len, Escapes) ->
    unescape(Bin, Start, Start + Len, Escapes, []).

%% The escapes are walked from last to first, so the slices between the dropped
%% quotes come out in order without reversing.
-spec unescape(binary(), non_neg_integer(), non_neg_integer(), escapes(), [binary()]) -> binary().
unescape(Bin, Start, End, [Drop | Escapes], Acc) ->
    unescape(Bin, Start, Drop, Escapes, [binary_part(Bin, Drop + 1, End - Drop - 1) | Acc]);
unescape(Bin, Start, End, [], Acc) ->
    iolist_to_binary([binary_part(Bin, Start, End - Start) | Acc]).

%% The chunk ended inside the row that started at RowStart: hand everything from
%% there back to the caller so it can be prepended to the next chunk.
-spec trailer(binary(), non_neg_integer(), [row()], csv_decoder()) -> decoded().
trailer(Bin, RowStart, [], State) ->
    %% No complete row was produced. Keep the historical distinction: when the
    %% chunk holds no separator nor delimiter at all it is a `nomatch', otherwise
    %% it is a partial row worth carrying over as a trailer.
    #csv_decoder{separator = S, delimiter = D, delimiter_rest = DelimiterRest} = State,
    Trailer = binary_part(Bin, RowStart, byte_size(Bin) - RowStart),
    case binary:match(Bin, [<<S>>, <<D, DelimiterRest/binary>>]) of
        nomatch -> {nomatch, Trailer};
        _ -> {has_trailer, [], Trailer}
    end;
trailer(Bin, RowStart, Rows, _State) ->
    {has_trailer, lists:reverse(Rows), binary_part(Bin, RowStart, byte_size(Bin) - RowStart)}.
