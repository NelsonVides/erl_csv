-module(erl_csv_decoder).

-include("erl_csv.hrl").

-export([decode/2]).

-record(csv_decoder, {
          line_break = ?DELIMITER :: <<_:8>> | <<_:16>>,
          separator  = ?SEPARATOR :: <<_:8>>,
          quotes     = ?QUOTES    :: <<_:8>>
         }).

-spec decode(iolist() | list(map()), erl_csv:decode_opts()) ->
    {ok, iolist()} | {error, iolist()}.
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

process_match(nomatch, _, Chunk) ->
    {error, Chunk};
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
