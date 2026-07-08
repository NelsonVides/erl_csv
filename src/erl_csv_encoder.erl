-module(erl_csv_encoder).
-moduledoc false.

-include("erl_csv.hrl").

-export([encode/2]).

-spec encode(iolist() | list(map()), erl_csv:encode_opts()) -> iolist().
encode([], _Opts) ->
    [];
encode([First | _] = Input, Opts) ->
    Separator = maps:get(separator, Opts, ?SEPARATOR),
    Delimiter = maps:get(delimiter, Opts, ?DELIMITER),
    %% Compile the set of characters that force a field to be quoted once per
    %% call instead of once per cell (which is what re:run/3 used to do).
    Reserved = reserved_pattern(Separator, Delimiter),
    case maps:get(headers, Opts, false) of
        false ->
            lists:map(
                fun(Row) -> encode_row(Row, Separator, Delimiter, Reserved, false) end, Input
            );
        true ->
            [
                encode_row(maps:keys(First), Separator, Delimiter, Reserved, false)
                | lists:map(
                    fun(Row) -> encode_row(Row, Separator, Delimiter, Reserved, true) end, Input
                )
            ];
        Headers when is_list(Headers) ->
            lists:map(
                fun(Row) -> encode_row(Row, Separator, Delimiter, Reserved, Headers) end, Input
            )
    end.

-spec reserved_pattern(binary(), binary()) -> binary:cp().
reserved_pattern(Separator, Delimiter) ->
    binary:compile_pattern(
        lists:usort([Separator, Delimiter, ?CARRIAGE_RETURN, ?NEWLINE, ?QUOTES])
    ).

encode_row(Row, Separator, Delimiter, Reserved, false) ->
    EncodedCells = encode_cells(Row, Reserved),
    Encoded = intersperse_rev(Separator, EncodedCells),
    lists:reverse([Delimiter | Encoded]);
encode_row(Row, Separator, Delimiter, Reserved, Headers) ->
    EncodedCells = encode_cells(get_values(Row, Headers), Reserved),
    Encoded = intersperse_rev(Separator, EncodedCells),
    lists:reverse([Delimiter | Encoded]).

get_values(Row, true) ->
    maps:values(Row);
get_values(Row, Headers) ->
    lists:map(fun(H) -> maps:get(H, Row) end, Headers).

encode_cells(Row, Reserved) when is_tuple(Row) ->
    lists:map(fun(Cell) -> encode_cell(Cell, Reserved) end, tuple_to_list(Row));
encode_cells(Row, Reserved) when is_list(Row) ->
    lists:map(fun(Cell) -> encode_cell(Cell, Reserved) end, Row).

intersperse_rev(Sep, Input) ->
    intersperse_rev(Sep, Input, []).

intersperse_rev(_, [], Acc) ->
    Acc;
intersperse_rev(Sep, [In], Acc) ->
    intersperse_rev(Sep, [], [In | Acc]);
intersperse_rev(Sep, [In | Rest], Acc) ->
    intersperse_rev(Sep, Rest, [Sep, In | Acc]).

encode_cell(Cell, Reserved) when is_binary(Cell) ->
    case binary:match(Cell, Reserved) of
        nomatch ->
            Cell;
        _ ->
            [
                ?QUOTES,
                binary:replace(Cell, ?QUOTES, <<?QUOTES/binary, ?QUOTES/binary>>, [global]),
                ?QUOTES
            ]
    end;
encode_cell(Cell, _Reserved) when is_tuple(Cell) ->
    [?QUOTES, io_lib:format("~p", [Cell]), ?QUOTES];
encode_cell(Cell, Reserved) when is_list(Cell) ->
    encode_cell(unicode:characters_to_binary(Cell), Reserved);
encode_cell(Cell, _Reserved) when is_integer(Cell) ->
    integer_to_binary(Cell);
encode_cell(Cell, _Reserved) when is_float(Cell) ->
    io_lib:format("~f", [Cell]);
encode_cell(Cell, _Reserved) when is_atom(Cell) ->
    io_lib:write_atom(Cell);
encode_cell(Cell, _Reserved) ->
    io_lib:format("\"~p\"", [Cell]).
