%% @private
-module(erl_csv_encoder).

-include("erl_csv.hrl").

-export([encode/2]).

-spec encode(iolist() | list(map()), erl_csv:encode_opts()) -> iolist().
encode([], _Opts) ->
    [];
encode([First | _ ] = Input, Opts) ->
    Separator = maps:get(separator, Opts, ?SEPARATOR),
    Delimiter = maps:get(delimiter, Opts, ?DELIMITER),
    case maps:get(headers, Opts, false) of
        false ->
            lists:map(fun(Row) -> encode_row(Row, Separator, Delimiter, false) end, Input);
        true ->
            [ encode_row(maps:keys(First), Separator, Delimiter, false)
              | lists:map(fun(Row) -> encode_row(Row, Separator, Delimiter, true) end, Input)];
        Headers when is_list(Headers) ->
            lists:map(fun(Row) -> encode_row(Row, Separator, Delimiter, Headers) end, Input)
    end.

encode_row(Row, Separator, Delimiter, false) ->
    EncodedCells = encode_cells(Row, Separator, Delimiter),
    Encoded = intersperse_rev(Separator, EncodedCells),
    lists:reverse([Delimiter | Encoded]);
encode_row(Row, Separator, Delimiter, Headers) ->
    EncodedCells = encode_cells(get_values(Row, Headers), Separator, Delimiter),
    Encoded = intersperse_rev(Separator, EncodedCells),
    lists:reverse([Delimiter | Encoded]).

get_values(Row, true) ->
    maps:values(Row);
get_values(Row, Headers) ->
    lists:map(fun(H) -> maps:get(H, Row) end, Headers).

encode_cells(Row, Sep, Del) when is_map(Row) ->
    maps:map(fun(_H, Cell) -> encode_cell(Cell, Sep, Del) end, Row);
encode_cells(Row, Sep, Del) when is_tuple(Row) ->
    lists:map(fun(Cell) -> encode_cell(Cell, Sep, Del) end, tuple_to_list(Row));
encode_cells(Row, Sep, Del) when is_list(Row) ->
    lists:map(fun(Cell) -> encode_cell(Cell, Sep, Del) end, Row).

intersperse_rev(Sep, Input) ->
    intersperse_rev(Sep, Input, []).

intersperse_rev(_, [], Acc) ->
    Acc;
intersperse_rev(Sep, [In], Acc) ->
    intersperse_rev(Sep, [], [In | Acc]);
intersperse_rev(Sep, [In | Rest], Acc) ->
    intersperse_rev(Sep, Rest, [ Sep, In | Acc ]).

encode_cell(Cell, Sep, Del) when is_binary(Cell) ->
    case re:run(Cell, ["[", Sep, Del, ?CARRIAGE_RETURN, ?NEWLINE, ?QUOTES, "]"], [global, unicode]) of
        nomatch ->
            Cell;
        _ ->
            [$", re:replace(Cell, <<$">>, <<"\"\"">>, [global, unicode]), $"]
    end;
encode_cell(Cell, _, _) when is_tuple(Cell) ->
    [$", io_lib:format("~p", [Cell]), $"];
encode_cell(Cell, Sep, Del) when is_list(Cell) ->
    encode_cell(unicode:characters_to_binary(Cell), Sep, Del);
encode_cell(Cell, _, _) when is_integer(Cell) ->
    integer_to_list(Cell);
encode_cell(Cell, _, _) when is_float(Cell) ->
    io_lib:format("~f",[Cell]);
encode_cell(Cell, _, _) when is_atom(Cell) ->
    io_lib:write_atom(Cell);
encode_cell(Cell, _, _) ->
    io_lib:format("\"~p\"",[Cell]).
