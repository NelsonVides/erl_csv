-module(erl_csv_decoder).

-include("erl_csv.hrl").

-export([decode/2, decode_new_s/2, decode_s/1]).

-record(csv_decoder, {
          line_break = ?DELIMITER :: <<_:8>> | <<_:16>>,
          separator  = ?SEPARATOR :: <<_:8>>,
          quotes     = ?QUOTES    :: <<_:8>>
         }).
-type csv_decoder() :: #csv_decoder{}.

-spec decode(iolist() | list(map()), erl_csv:decode_opts()) ->
    {ok, iolist()} | {has_trailer, iolist(), iolist()} | {nomatch, iolist()}.
decode(Chunk, Opts) ->
    Separator = maps:get(separator, Opts, ?SEPARATOR),
    Delimiter = maps:get(delimiter, Opts, ?DELIMITER),
    Quotes = maps:get(qoutes, Opts, ?QUOTES),
    {ok, Regex} = re:compile(
                    <<"(", Quotes/binary, ")?",
                      "(?(1)",
                      "((", Quotes/binary, "{2}|[^", Quotes/binary, "])*", Quotes/binary, ")",
                      "|[^", Quotes/binary, Separator/binary, Delimiter/binary,"]*)",
                      "(", Separator/binary, "|", Delimiter/binary, ")">>),
    State = #csv_decoder{separator = Separator, line_break = Delimiter, quotes = Quotes},
    Match = re:run(Chunk, Regex, [global, {capture, all, index}]),
    process_match(Match, State, Chunk).

-spec decode_new_s(file:name_all(), erl_csv:decode_opts()) ->
    {ok, csv_stream()} | {error, term()}.
decode_new_s(File, Opts) ->
    case erl_csv_file_stream:read_file(File, Opts) of
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
            {ok, Decoded, Stream#csv_stream{hd = []}};
        {has_trailer, Decoded, Trailer} ->
            SavedStream = Stream#csv_stream{hd = Trailer},
            get_more_stream(SavedStream);
        {nomatch, _} ->
            decode_s(get_more_stream(Stream));
        {error, Reason} ->
            {error, Reason}
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

-spec process_match(any(), csv_decoder(), iolist()) ->
    {ok, iolist()} | {has_trailer, iolist(), iolist()} | {nomatch, iolist()}.
process_match(nomatch, _, NotMatched) ->
    {nomatch, NotMatched};
process_match({match, Matches}, State, Chunk) ->
    case process_chunk(Matches, State, Chunk) of
        {Decoded, <<>>} -> {ok, Decoded};
        {Decoded, Trailer} -> {has_trailer, Decoded, Trailer}
    end.

process_chunk(Matches, #csv_decoder{line_break = LineBreak} = State, Chunk) ->
    Matches2 = filter_incomplete_lines(Matches, Chunk, LineBreak),
    process_chunk(Matches2, Chunk, State, [], [], 0).

process_chunk([], Chunk, #csv_decoder{}, [], Acc, LenProcessed) ->
    Size = byte_size(Chunk),
    NotProcessed = Size - LenProcessed,
    NewChunk = binary:part(Chunk, Size, - NotProcessed),
    {lists:reverse(Acc), NewChunk};
process_chunk([ [{Pos, Len} | _] | Matches], Chunk,
              #csv_decoder{line_break = LineBreak, separator = SepBy} = State, LineAcc, Acc, _) ->
    Csv = binary:part(Chunk, Pos, Len - 1),
    Csv2 = format_term(Csv, State),
    case binary:part(Chunk, Pos + Len, - 1) of
        LineBreak -> NewLine = lists:reverse([Csv2 | LineAcc]),
                     process_chunk(Matches, Chunk, State, [], [NewLine | Acc], Pos + Len);
        SepBy -> process_chunk(Matches, Chunk, State, [Csv2 | LineAcc], Acc, Pos + Len)
    end.

filter_incomplete_lines(Matches, Chunk, LineBreak) ->
    Fun = fun(Match) ->
                  PosLen = lists:last(Match),
                  binary:part(Chunk, PosLen) =/= LineBreak
          end,
    lists:reverse(lists:dropwhile(Fun, lists:reverse(Matches))).

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
