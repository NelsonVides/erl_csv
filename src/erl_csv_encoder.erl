-module(erl_csv_encoder).
-moduledoc false.

-include("erl_csv.hrl").

-export([encode/2]).

-record(csv_encoder, {
    separator :: binary(),
    delimiter :: binary(),
    %% Any of these in a field forces it to be quoted.
    reserved :: binary:cp(),
    quote :: binary:cp()
}).
-type csv_encoder() :: #csv_encoder{}.

-spec encode(iolist() | list(map()), erl_csv:encode_opts()) -> iolist().
encode([], _Opts) ->
    [];
encode([First | _] = Input, Opts) ->
    Separator = maps:get(separator, Opts, ?SEPARATOR),
    Delimiter = maps:get(delimiter, Opts, ?DELIMITER),
    %% Patterns are compiled once per call instead of once per cell.
    State = #csv_encoder{
        separator = Separator,
        delimiter = Delimiter,
        reserved = reserved_pattern(Separator, Delimiter),
        quote = binary:compile_pattern(?QUOTES)
    },
    case maps:get(headers, Opts, false) of
        false ->
            encode_rows(Input, State);
        true ->
            encode_maps(maps:keys(First), Input, State);
        Headers when is_list(Headers) ->
            encode_maps(Headers, Input, State)
    end.

%% The header row first, then the value of each header in each map, looked up by
%% key so that every value lands in its own column.
-spec encode_maps(list(), [map()], csv_encoder()) -> iolist().
encode_maps(Headers, Input, State) ->
    Rows = lists:map(fun(Row) -> get_values(Row, Headers) end, Input),
    [encode_row(Headers, State) | encode_rows(Rows, State)].

-spec get_values(map(), list()) -> list().
get_values(Row, Headers) ->
    lists:map(fun(H) -> maps:get(H, Row) end, Headers).

-spec reserved_pattern(binary(), binary()) -> binary:cp().
reserved_pattern(Separator, Delimiter) ->
    binary:compile_pattern(
        lists:usort([Separator, Delimiter, ?CARRIAGE_RETURN, ?NEWLINE, ?QUOTES])
    ).

-spec encode_rows([list() | tuple()], csv_encoder()) -> iolist().
encode_rows([Row | Rows], State) ->
    [encode_row(Row, State) | encode_rows(Rows, State)];
encode_rows([], _State) ->
    [].

%% Each row comes out as a flat `[Cell, Separator, Cell, ..., Delimiter]', built
%% front to back so no intermediate list has to be reversed.
-spec encode_row(list() | tuple(), csv_encoder()) -> iolist().
encode_row(Row, State) when is_tuple(Row) ->
    encode_row(tuple_to_list(Row), State);
encode_row([Cell | Cells], State) ->
    [encode_cell(Cell, State) | encode_row_rest(Cells, State)];
encode_row([], #csv_encoder{delimiter = Delimiter}) ->
    [Delimiter].

-spec encode_row_rest(list(), csv_encoder()) -> iolist().
encode_row_rest([Cell | Cells], #csv_encoder{separator = Separator} = State) ->
    [Separator, encode_cell(Cell, State) | encode_row_rest(Cells, State)];
encode_row_rest([], #csv_encoder{delimiter = Delimiter}) ->
    [Delimiter].

-spec encode_cell(term(), csv_encoder()) -> iodata().
encode_cell(Cell, #csv_encoder{reserved = Reserved} = State) when is_binary(Cell) ->
    case binary:match(Cell, Reserved) of
        nomatch ->
            Cell;
        _ ->
            quoted(Cell, State)
    end;
encode_cell(Cell, State) when is_list(Cell) ->
    encode_cell(unicode:characters_to_binary(Cell), State);
encode_cell(Cell, _State) when is_integer(Cell) ->
    integer_to_binary(Cell);
encode_cell(Cell, _State) when is_float(Cell) ->
    %% The shortest text that reads back as the same float.
    float_to_binary(Cell, [short]);
encode_cell(Cell, State) when is_atom(Cell) ->
    encode_cell(unicode:characters_to_binary(io_lib:write_atom(Cell)), State);
encode_cell(Cell, State) ->
    %% Tuples and any other term are written in Erlang syntax, always quoted.
    quoted(unicode:characters_to_binary(io_lib:format("~tp", [Cell])), State).

%% Double every quote by splitting the cell on quotes and joining the slices with
%% doubled quotes: much cheaper than binary:replace/4, which is plain Erlang
%% built on top of binary:matches/3.
-spec quoted(binary(), csv_encoder()) -> iodata().
quoted(Cell, #csv_encoder{quote = Quote}) ->
    case binary:match(Cell, Quote) of
        nomatch ->
            [?QUOTES, Cell, ?QUOTES];
        _ ->
            Slices = binary:split(Cell, Quote, [global]),
            [?QUOTES, lists:join(<<?QUOTES/binary, ?QUOTES/binary>>, Slices), ?QUOTES]
    end.
