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

-spec decode(iodata(), erl_csv:decode_opts()) ->
    {ok, iodata()} | {has_trailer, iodata(), iodata()} | {nomatch, iodata()} | {error, term()}.
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
    {ok, iodata(), csv_stream()} | {error, term()}.
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
            SavedStream = Stream#csv_stream{hd = [Bin, NotMatched]},
            decode_s(get_more_stream(SavedStream));
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

-spec process_match(
        {match, matches()} | {error, term()} | match | nomatch, csv_decoder(), iodata()) ->
    {ok, iodata()} | {has_trailer, iodata(), iodata()} | {nomatch, iodata()} | {error, term()}.
process_match({match, [ [{_, _} | _] | _] = Matches}, State, Chunk) ->
    case pre_process_chunk(Matches, State, iolist_to_binary(Chunk)) of
        {no_trailer, Decoded} ->
            {ok, Decoded};
        {has_trailer, Decoded, Trailer} ->
            {has_trailer, Decoded, Trailer}
    end;
process_match({error, Reason}, _, _) ->
    {error, Reason};
process_match(_, _, NotMatched) ->
    {nomatch, NotMatched}.

-spec pre_process_chunk([matches()], csv_decoder(), binary()) ->
    {no_trailer, Decoded :: iodata()} |
    {has_trailer, Decoded :: iodata(), Trailer :: iodata()}.
pre_process_chunk(Matches, #csv_decoder{line_break = LineBreak} = State, Chunk) ->
    {Matches2, _Filtered} = filter_incomplete_lines(Matches, Chunk, LineBreak),
    process_chunk(Matches2, Chunk, State, [], [], 0).

-spec process_chunk([matches()], binary(), csv_decoder(), list(), list(), non_neg_integer()) ->
    {no_trailer, Match :: iodata()} |
    {has_trailer, Match :: iodata(), Trailer :: iodata()}.
process_chunk([], Chunk, #csv_decoder{}, [], Acc, LenProcessed) ->
    Size = byte_size(Chunk),
    NotProcessed = Size - LenProcessed,
    NewChunk = binary:part(Chunk, Size, - NotProcessed),
    case iolist_size(NewChunk) of
        0 -> {no_trailer, lists:reverse(Acc)};
        _ -> {has_trailer, lists:reverse(Acc), NewChunk}
    end;
process_chunk([ [{Pos, Len} | _] | Matches], Chunk,
              #csv_decoder{line_break = LineBreak, separator = SepBy} = State, LineAcc, Acc, _) ->
    Csv = binary:part(Chunk, Pos, Len - 1),
    Csv2 = format_term(Csv, State),
    case binary:part(Chunk, Pos + Len, - 1) of
        LineBreak -> NewLine = lists:reverse([Csv2 | LineAcc]),
                     process_chunk(Matches, Chunk, State, [], [NewLine | Acc], Pos + Len);
        SepBy -> process_chunk(Matches, Chunk, State, [Csv2 | LineAcc], Acc, Pos + Len)
    end.

-spec filter_incomplete_lines([matches()], binary(), <<_:8>>) ->
    {[matches()], [matches()]}.
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
