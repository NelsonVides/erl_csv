-module(erl_csv_decoder).
-moduledoc false.

-include("erl_csv.hrl").

-export([decode/2, decode_new_s/2, decode_s/1]).

-record(csv_decoder, {
    line_break = ?DELIMITER :: binary(),
    line_break_size = 1 :: pos_integer(),
    separator = ?SEPARATOR :: <<_:8>>,
    quotes = ?QUOTES :: <<_:8>>,
    quote_byte = $" :: byte(),
    terminators :: binary:cp() | undefined
}).
-type csv_decoder() :: #csv_decoder{}.

%% Result of parsing a single field: which token terminated it, the field value,
%% and the offset right after the terminator. `incomplete' means the field (and
%% therefore its row) could not be completed within this chunk.
-type field_result() ::
    {separator | delimiter, binary(), non_neg_integer()} | incomplete.

-spec decode(iodata(), erl_csv:decode_opts()) ->
    {ok, iodata()} | {has_trailer, iodata(), iodata()} | {nomatch, iodata()}.
decode(Chunk, Opts) ->
    Bin = iolist_to_binary(Chunk),
    Separator = maps:get(separator, Opts, ?SEPARATOR),
    Delimiter = maps:get(delimiter, Opts, ?DELIMITER),
    Quotes = maps:get(quotes, Opts, ?QUOTES),
    <<QuoteByte>> = Quotes,
    State = #csv_decoder{
        separator = Separator,
        line_break = Delimiter,
        line_break_size = byte_size(Delimiter),
        quotes = Quotes,
        quote_byte = QuoteByte,
        terminators = binary:compile_pattern([Separator, Delimiter])
    },
    decode_rows(Bin, 0, byte_size(Bin), State, []).

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
                MoreStream -> {ok, Decoded, MoreStream}
            end;
        {nomatch, NotMatched} ->
            SavedStream = Stream#csv_stream{hd = NotMatched},
            decode_s(get_more_stream(SavedStream))
    end;
decode_s({error, Reason}) ->
    {error, Reason}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_more_stream(erl_csv:csv_stream()) -> erl_csv:maybe_csv_stream().
get_more_stream(Stream) ->
    case erl_csv_file_stream:tl(Stream) of
        {error, Reason} ->
            {error, Reason};
        NewStream ->
            NewStream
    end.

%% Parse as many complete rows (rows terminated by an unquoted delimiter) as the
%% chunk holds. Anything from the start of the first incomplete row onwards is
%% returned to the caller as a trailer so it can be prepended to the next chunk.
-spec decode_rows(binary(), non_neg_integer(), non_neg_integer(), csv_decoder(), [[binary()]]) ->
    {ok, [[binary()]]} | {has_trailer, [[binary()]], binary()} | {nomatch, binary()}.
decode_rows(Bin, Pos, Size, _State, Acc) when Pos >= Size ->
    case Acc of
        [] -> {nomatch, Bin};
        _ -> {ok, lists:reverse(Acc)}
    end;
decode_rows(Bin, Pos, Size, State, Acc) ->
    case decode_row(Bin, Pos, Pos, Size, State, []) of
        {row, Fields, Next} ->
            decode_rows(Bin, Next, Size, State, [Fields | Acc]);
        {trailer, RowStart} ->
            Trailer = binary:part(Bin, RowStart, Size - RowStart),
            trailer_result(Bin, Trailer, State, Acc)
    end.

-spec trailer_result(binary(), binary(), csv_decoder(), [[binary()]]) ->
    {has_trailer, [[binary()]], binary()} | {nomatch, binary()}.
trailer_result(Bin, Trailer, #csv_decoder{terminators = Terminators}, []) ->
    %% No complete row was produced. Keep the historical distinction: when the
    %% chunk holds no separator nor delimiter at all it is a `nomatch', otherwise
    %% it is a partial row worth carrying over as a trailer.
    case binary:match(Bin, Terminators) of
        nomatch -> {nomatch, Trailer};
        _ -> {has_trailer, [], Trailer}
    end;
trailer_result(_Bin, Trailer, _State, Acc) ->
    {has_trailer, lists:reverse(Acc), Trailer}.

-spec decode_row(
    binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), csv_decoder(), [binary()]
) ->
    {row, [binary()], non_neg_integer()} | {trailer, non_neg_integer()}.
decode_row(Bin, RowStart, Pos, Size, State, Fields) ->
    case decode_field(Bin, Pos, Size, State) of
        {separator, Field, Next} ->
            decode_row(Bin, RowStart, Next, Size, State, [Field | Fields]);
        {delimiter, Field, Next} ->
            {row, lists:reverse([Field | Fields]), Next};
        incomplete ->
            {trailer, RowStart}
    end.

-spec decode_field(binary(), non_neg_integer(), non_neg_integer(), csv_decoder()) ->
    field_result().
decode_field(_Bin, Pos, Size, _State) when Pos >= Size ->
    %% A separator was the last byte of the chunk: the trailing field (and its
    %% row) is incomplete until more data arrives.
    incomplete;
decode_field(Bin, Pos, Size, #csv_decoder{quote_byte = QuoteByte} = State) ->
    case binary:at(Bin, Pos) of
        QuoteByte -> decode_quoted_field(Bin, Pos + 1, Pos + 1, Size, State);
        _ -> decode_unquoted_field(Bin, Pos, Size, State)
    end.

-spec decode_unquoted_field(binary(), non_neg_integer(), non_neg_integer(), csv_decoder()) ->
    field_result().
decode_unquoted_field(Bin, Pos, Size, #csv_decoder{terminators = Terminators} = State) ->
    case binary:match(Bin, Terminators, [{scope, {Pos, Size - Pos}}]) of
        nomatch ->
            incomplete;
        {TermPos, TermLen} ->
            Field = binary:part(Bin, Pos, TermPos - Pos),
            {kind_of_terminator(Bin, TermPos, TermLen, State), Field, TermPos + TermLen}
    end.

%% A field that opens with the quote character. Scan for the matching closing
%% quote, treating a doubled quote (`""') as an escaped literal quote.
-spec decode_quoted_field(
    binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), csv_decoder()
) ->
    field_result().
decode_quoted_field(Bin, ContentStart, SearchPos, Size, #csv_decoder{quotes = Q} = State) ->
    case binary:match(Bin, Q, [{scope, {SearchPos, Size - SearchPos}}]) of
        nomatch ->
            incomplete;
        {QuotePos, _} ->
            AfterQuote = QuotePos + 1,
            case is_escaped_quote(Bin, AfterQuote, Size, Q) of
                true ->
                    decode_quoted_field(Bin, ContentStart, AfterQuote + 1, Size, State);
                false ->
                    close_quoted_field(Bin, ContentStart, QuotePos, AfterQuote, Size, State)
            end
    end.

-spec close_quoted_field(
    binary(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    csv_decoder()
) ->
    field_result().
close_quoted_field(Bin, ContentStart, QuotePos, AfterQuote, Size, State) ->
    case terminator_at(Bin, AfterQuote, Size, State) of
        {Kind, Next} ->
            {Kind, unescape(Bin, ContentStart, QuotePos, State), Next};
        eof ->
            %% Closing quote but no terminator yet: the row is not complete.
            incomplete;
        garbage ->
            %% Content between the closing quote and the terminator is malformed;
            %% fall back to reading the whole field (from the opening quote)
            %% verbatim rather than dropping data.
            decode_unquoted_field(Bin, ContentStart - 1, Size, State)
    end.

-spec is_escaped_quote(binary(), non_neg_integer(), non_neg_integer(), <<_:8>>) -> boolean().
is_escaped_quote(_Bin, AfterQuote, Size, _Q) when AfterQuote >= Size ->
    false;
is_escaped_quote(Bin, AfterQuote, _Size, Q) ->
    binary:part(Bin, AfterQuote, 1) =:= Q.

%% Classify the terminator that binary:match/3 landed on for an unquoted field.
-spec kind_of_terminator(binary(), non_neg_integer(), non_neg_integer(), csv_decoder()) ->
    separator | delimiter.
kind_of_terminator(_Bin, _TermPos, TermLen, #csv_decoder{line_break_size = DelSize}) when
    TermLen =/= DelSize
->
    %% Different length than the delimiter, so it can only be the separator.
    separator;
kind_of_terminator(Bin, TermPos, TermLen, #csv_decoder{line_break = Delimiter}) ->
    case binary:part(Bin, TermPos, TermLen) of
        Delimiter -> delimiter;
        _ -> separator
    end.

%% Determine what, if anything, terminates a field at the given position.
-spec terminator_at(binary(), non_neg_integer(), non_neg_integer(), csv_decoder()) ->
    {separator | delimiter, non_neg_integer()} | eof | garbage.
terminator_at(_Bin, Pos, Size, _State) when Pos >= Size ->
    eof;
terminator_at(Bin, Pos, Size, #csv_decoder{
    separator = Sep, line_break = Delimiter, line_break_size = DelSize
}) ->
    case Pos + DelSize =< Size andalso binary:part(Bin, Pos, DelSize) =:= Delimiter of
        true ->
            {delimiter, Pos + DelSize};
        false ->
            SepSize = byte_size(Sep),
            case binary:part(Bin, Pos, SepSize) =:= Sep of
                true -> {separator, Pos + SepSize};
                false -> garbage
            end
    end.

-spec unescape(binary(), non_neg_integer(), non_neg_integer(), csv_decoder()) -> binary().
unescape(Bin, ContentStart, QuotePos, #csv_decoder{quotes = Q}) ->
    Raw = binary:part(Bin, ContentStart, QuotePos - ContentStart),
    binary:replace(Raw, <<Q/binary, Q/binary>>, Q, [global]).
