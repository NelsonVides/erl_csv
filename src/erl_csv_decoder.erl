%% @private
-module(erl_csv_decoder).

-include("erl_csv.hrl").

-export([decode/2, decode_new_s/2, decode_s/1]).

-record(csv_decoder, {
          line_break = ?DELIMITER :: <<_:8>> | <<_:16>>,
          separator  = ?SEPARATOR :: <<_:8>>,
          quotes     = ?QUOTES    :: <<_:8>>
         }).
-type csv_decoder() :: #csv_decoder{}.
-type matches() :: [{non_neg_integer(), non_neg_integer()}].

% -type match() :: nomatch | {match, matches()}.
% -spec decode(binary(), erl_csv:decode_opts()) ->
%     {ok, binary()} | {has_trailer, iolist(), binary()} | {nomatch, binary()}.
decode(Chunk, Opts) ->
    Separator = maps:get(separator, Opts, ?SEPARATOR),
    Delimiter = maps:get(delimiter, Opts, ?DELIMITER),
    Quotes = maps:get(qoutes, Opts, ?QUOTES),
    Regex = build_regex(Opts),
    State = #csv_decoder{separator = Separator, line_break = Delimiter, quotes = Quotes},
    Match = re:run(Chunk, Regex, [global, {capture, all, index}]),
    process_match(Match, State, Chunk).

-spec decode_new_s(file:name_all(), erl_csv:decode_opts()) ->
    {ok, csv_stream()} | {error, term()}.
decode_new_s(File, Opts) ->
    Opts1 = Opts#{regex => build_regex(Opts)},
    case erl_csv_file_stream:read_file(File, Opts1) of
        #csv_stream{} = Stream ->
            {ok, Stream};
        {error, Reason} ->
            {error, Reason}
    end.

-spec decode_s(maybe_csv_stream()) ->
    {ok, iolist(), csv_stream()} | {error, term()}.
decode_s(stream_end) ->
    {ok, [], stream_end};
decode_s(#csv_stream{hd = Bin, opts = Opts} = Stream) ->
    case decode(Bin, Opts) of
        {ok, Decoded} ->
            {ok, Decoded, Stream#csv_stream{hd = <<>>}};
        {has_trailer, Decoded, Trailer} ->
            SavedStream = Stream#csv_stream{hd = Trailer},
            {ok, Decoded, get_more_stream(SavedStream)};
        {nomatch, NotMatched} ->
            SavedStream = Stream#csv_stream{hd = iolist_to_binary([Bin, NotMatched])},
            decode_s(get_more_stream(SavedStream))
    end;
decode_s({error, Reason}) ->
    {error, Reason}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_more_stream(csv_stream()) -> maybe_csv_stream().
get_more_stream(Stream) ->
    case erl_csv_file_stream:tl(Stream) of
        {error, Reason} ->
            {error, Reason};
        NewStream ->
            NewStream
    end.

% -spec process_match({error, term()} | match(), csv_decoder(), binary()) ->
%     {ok, binary()} | {has_trailer, iolist(), binary()} | {nomatch, binary()} | {error, term()}.
process_match({error, Reason}, _, _) ->
    {error, Reason};
process_match(nomatch, _, NotMatched) ->
    {nomatch, NotMatched};
process_match({match, Matches}, State, Chunk) ->
    case pre_process_chunk(Matches, State, Chunk) of
        {Decoded, no_trailer} -> {ok, Decoded};
        {Decoded, Trailer} when is_binary(Trailer) -> {has_trailer, Decoded, Trailer}
    end.

-spec pre_process_chunk([matches()], csv_decoder(), binary()) ->
    {iolist(), no_trailer | binary()}.
pre_process_chunk(Matches, #csv_decoder{line_break = LineBreak} = State, Chunk) ->
    {Matches2, Filtered} = filter_incomplete_lines(Matches, Chunk, LineBreak),
    process_chunk(Matches2, Chunk, Filtered, State, [], [], 0).

-spec process_chunk([matches()], binary(), list(), csv_decoder(), list(), list(), non_neg_integer()) ->
    {iolist(), no_trailer | binary()}.
process_chunk([], Chunk, Filtered, #csv_decoder{}, [], Acc, LenProcessed) ->
    Size = byte_size(Chunk),
    NotProcessed = Size - LenProcessed,
    NewChunk = binary:part(Chunk, Size, - NotProcessed),
    case iolist_to_binary([NewChunk, Filtered]) of
        <<>> -> {lists:reverse(Acc), no_trailer};
        Trailer -> {lists:reverse(Acc), Trailer}
    end;
process_chunk([ [{Pos, Len} | _] | Matches], Chunk,
              Filtered, #csv_decoder{line_break = LineBreak, separator = SepBy} = State, LineAcc, Acc, _) ->
    Csv = binary:part(Chunk, Pos, Len - 1),
    Csv2 = format_term(Csv, State),
    case binary:part(Chunk, Pos + Len, - 1) of
        LineBreak -> NewLine = lists:reverse([Csv2 | LineAcc]),
                     process_chunk(Matches, Chunk, Filtered, State, [], [NewLine | Acc], Pos + Len);
        SepBy -> process_chunk(Matches, Chunk, Filtered, State, [Csv2 | LineAcc], Acc, Pos + Len)
    end.

-spec filter_incomplete_lines([matches()], binary(), <<_:8>>) ->
    {iolist(), iolist()}.
filter_incomplete_lines(Matches, Chunk, LineBreak) ->
    Fun = fun(Match) ->
                  PosLen = lists:last(Match),
                  binary:part(Chunk, PosLen) =/= LineBreak
          end,
    {Filtered, Matches2} = lists:splitwith(Fun, lists:reverse(Matches)),
    {lists:reverse(Matches2), Filtered}.

-spec format_term(binary(), csv_decoder()) -> binary().
format_term(CsvTerm, #csv_decoder{quotes = Q}) ->
    Term1 = case CsvTerm of
                <<Q:1/binary, Rest/binary>> ->
                    Size = byte_size(Rest) - 1,
                    <<Term:Size/binary, Q/binary>> = Rest,
                    Term;
                _ ->
                    CsvTerm
            end,
    binary:replace(Term1, <<Q/binary, Q/binary>>, Q, [global]).

build_regex(Opts) ->
    Separator = maps:get(separator, Opts, ?SEPARATOR),
    Delimiter = maps:get(delimiter, Opts, ?DELIMITER),
    Quotes = maps:get(qoutes, Opts, ?QUOTES),
    case maps:get(regex, Opts, undefined) of
        undefined ->
            {ok, Regex0} = re:compile(
                             <<"(", Quotes/binary, ")?",
                               "(?(1)",
                               "((", Quotes/binary, "{2}|[^", Quotes/binary, "])*", Quotes/binary, ")",
                               "|[^", Quotes/binary, Separator/binary, Delimiter/binary,"]*)",
                               "(", Separator/binary, "|", Delimiter/binary, ")">>),
            Regex0;
        Regex0 -> Regex0
    end.
