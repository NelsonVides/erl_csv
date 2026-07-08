-module(csv_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        {group, encode},
        {group, decode},
        {group, options},
        {group, decode_edge},
        {group, nimble},
        {group, roundtrip}
    ].

groups() ->
    [
        {encode, [parallel], [
            quotes_and_newlines,
            escaped_quotes,
            utf8,
            json
        ]},
        {decode, [parallel], [
            quotes_and_newlines,
            escaped_quotes,
            utf8,
            json,
            incomplete_input,
            stream_equals_full,
            stream_with_new_lines
        ]},
        {options, [parallel], [
            custom_quotes,
            crlf_delimiter,
            utf8_roundtrip
        ]},
        % Table-driven edge cases plus the two malformed-input behaviours the
        % binary-matching decoder fixed.
        {decode_edge, [parallel], [
            decode_table,
            stray_quote_is_lossless,
            lone_quote_is_a_trailer
        ]},
        % Learning tests ported from nimble_csv, adapted to erl_csv's chunk API.
        {nimble, [parallel], [
            nimble_basic,
            nimble_without_trailing_newline,
            nimble_crlf,
            nimble_empty_string,
            nimble_blank_lines,
            nimble_whitespace,
            nimble_escapes,
            nimble_separator_inside_quotes,
            nimble_multiline_quoted,
            nimble_escaped_escapes,
            nimble_unterminated_quote
        ]},
        {roundtrip, [parallel], [
            roundtrip_special_chars,
            roundtrip_fuzz
        ]}
    ].

run_test(encode, Encoded, Decoded) ->
    Csv = unicode:characters_to_binary(erl_csv:encode(Decoded)),
    ?assertEqual(Encoded, Csv);
run_test(decode, Encoded, Decoded) ->
    {ok, Csv} = erl_csv:decode(Encoded),
    ?assertEqual(Decoded, Csv).

which_group(Config) ->
    GroupProps = ?config(tc_group_properties, Config),
    {name, Group} = lists:keyfind(name, 1, GroupProps),
    Group.

incomplete_input(_) ->
    Decoded = [[<<"1">>, <<"2">>]],
    Result = erl_csv:decode(<<"1,2\n3,4">>),
    ?assertEqual({has_trailer, Decoded, <<"3,4">>}, Result).

custom_quotes(_Config) ->
    % Regression: the `quotes' option used to be silently ignored because the
    % decoder looked up a misspelled `qoutes' key, so a custom quote character
    % was never stripped nor unescaped.
    Opts = #{quotes => <<$'>>},
    ?assertEqual(
        {ok, [[<<"a'b">>, <<"c">>]]},
        erl_csv:decode(<<"'a''b',c\n">>, Opts)
    ).

crlf_delimiter(_Config) ->
    % Regression: a multi-byte delimiter used to crash the decoder with a
    % case_clause because only the single byte following each field was
    % inspected to tell a separator apart from a line delimiter.
    Opts = #{delimiter => <<"\r\n">>},
    Rows = [[<<"a">>, <<"b">>], [<<"c">>, <<"d">>]],
    Encoded = unicode:characters_to_binary(erl_csv:encode(Rows, Opts)),
    ?assertEqual(<<"a,b\r\nc,d\r\n">>, Encoded),
    ?assertEqual({ok, Rows}, erl_csv:decode(Encoded, Opts)).

utf8_roundtrip(_Config) ->
    % A multi-byte field (é = 233, ☃ = 9731) that also contains a separator
    % must be quoted on encode and read back byte-for-byte on decode.
    Field = <<"caf", 233/utf8, ", ", 9731/utf8, " snow">>,
    Rows = [[Field, <<"plain">>]],
    Encoded = unicode:characters_to_binary(erl_csv:encode(Rows)),
    ?assertEqual(<<"\"caf", 233/utf8, ", ", 9731/utf8, " snow\",plain\n">>, Encoded),
    ?assertEqual({ok, Rows}, erl_csv:decode(Encoded)).

quotes_and_newlines(Config) ->
    % given
    Decoded = [
        [<<"a">>, <<"b">>],
        [<<"1">>, <<"ha, \n\"ha\", \nha">>],
        [<<"3">>, <<"4">>]
    ],
    Encoded = <<"a,b\n1,\"ha, \n\"\"ha\"\", \nha\"\n3,4\n">>,
    % then
    run_test(which_group(Config), Encoded, Decoded).

escaped_quotes(Config) ->
    % given
    Decoded = [
        [<<"a">>, <<"b">>],
        [<<"1">>, <<"ha \"ha\" ha">>],
        [<<"3">>, <<"4">>]
    ],
    Encoded = <<"a,b\n1,\"ha \"\"ha\"\" ha\"\n3,4\n">>,
    % when
    run_test(which_group(Config), Encoded, Decoded).

utf8(Config) ->
    % given
    Decoded = [
        [<<"a">>, <<13371/utf8>>, <<"c">>],
        [<<"1">>, <<"2">>, <<"3">>],
        [<<"4">>, <<"5">>, <<676/utf8>>]
    ],
    Encoded =
        <<97, 44, 227, 144, 187, 44, 99, 10, 49, 44, 50, 44, 51, 10, 52, 44, 53, 44, 202, 164, 10>>,
    % when
    run_test(which_group(Config), Encoded, Decoded).

json(Config) ->
    % given
    Decoded = [
        [<<"key">>, <<"val">>],
        [<<"1">>, <<"{\"type\": \"Point\",\"coordinates\": [102.0, 0.5]}">>]
    ],
    Encoded =
        <<"key,val\n1,\"{\"\"type\"\": \"\"Point\"\",\"\"coordinates\"\": [102.0, 0.5]}\"\n">>,
    % when
    run_test(which_group(Config), Encoded, Decoded).

stream_equals_full(Config) ->
    CsvFile = filename:join([?config(data_dir, Config), "csv_example.csv"]),
    match_file(CsvFile).

stream_with_new_lines(Config) ->
    CsvFile = filename:join([?config(data_dir, Config), "short_new_lines.csv"]),
    match_file(CsvFile).

match_file(CsvFile) ->
    {Worker, Ref} = spawn_monitor(fun() -> accumulate([]) end),
    {ok, Stream} = erl_csv:decode_new_s(CsvFile),
    Result = do_import(Stream, Worker, Ref),
    {ok, Bin} = file:read_file(CsvFile),
    {ok, Decoded} = erl_csv:decode(Bin),
    ?assertEqual(Decoded, Result).

do_import(stream_end, Worker, Ref) ->
    collect(Worker, Ref);
do_import(Stream, Worker, Ref) ->
    {ok, Decoded, MoreStream} = erl_csv:decode_s(Stream),
    Worker ! {csv, Decoded},
    do_import(MoreStream, Worker, Ref).

accumulate(Acc) ->
    receive
        stop ->
            exit(lists:reverse(Acc));
        %% A decode_s/1 step legitimately yields no rows when the chunk only
        %% extended a pending trailer (e.g. a quoted field spanning a newline),
        %% so an empty batch must not end accumulation: only `stop' does.
        {csv, Data} ->
            accumulate(lists:reverse(Data, Acc))
    end.

collect(Worker, Ref) ->
    Worker ! stop,
    receive
        {'DOWN', Ref, process, Worker, Result} ->
            Result
    after 5000 ->
        ct:fail("Message not received")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table-driven decode edge cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_table(_Config) ->
    Failures = lists:filtermap(
        fun({Name, Input, Opts, Expected}) ->
            Actual = erl_csv:decode(Input, Opts),
            case Actual =:= Expected of
                true -> false;
                false -> {true, #{case_name => Name, expected => Expected, actual => Actual}}
            end
        end,
        decode_cases()
    ),
    ?assertEqual([], Failures).

decode_cases() ->
    D = #{},
    [
        % {Name, Input, Opts, Expected}
        {empty, <<>>, D, {nomatch, <<>>}},
        {bare_newline, <<"\n">>, D, {ok, [[<<>>]]}},
        {single_field, <<"a\n">>, D, {ok, [[<<"a">>]]}},
        {two_fields, <<"a,b\n">>, D, {ok, [[<<"a">>, <<"b">>]]}},
        {two_rows, <<"a,b\nc,d\n">>, D, {ok, [[<<"a">>, <<"b">>], [<<"c">>, <<"d">>]]}},
        {three_fields, <<"a,b,c\n">>, D, {ok, [[<<"a">>, <<"b">>, <<"c">>]]}},
        {trailing_empty_field, <<"a,\n">>, D, {ok, [[<<"a">>, <<>>]]}},
        {two_empty_fields, <<",\n">>, D, {ok, [[<<>>, <<>>]]}},
        {no_terminator, <<"abc">>, D, {nomatch, <<"abc">>}},
        {partial_row, <<"a,b">>, D, {has_trailer, [], <<"a,b">>}},
        {partial_after_row, <<"a,b\nc,d">>, D, {has_trailer, [[<<"a">>, <<"b">>]], <<"c,d">>}},
        {dangling_separator, <<",">>, D, {has_trailer, [], <<",">>}},
        {quoted_separator, <<"\"a,b\",c\n">>, D, {ok, [[<<"a,b">>, <<"c">>]]}},
        {quoted_escape, <<"\"a\"\"b\",c\n">>, D, {ok, [[<<"a\"b">>, <<"c">>]]}},
        {quoted_newline, <<"\"a\nb\",c\n">>, D, {ok, [[<<"a\nb">>, <<"c">>]]}},
        {quoted_no_terminator, <<"\"a,b\"">>, D, {has_trailer, [], <<"\"a,b\"">>}},
        {empty_quoted, <<"\"\"\n">>, D, {ok, [[<<>>]]}},
        {two_empty_quoted, <<"\"\",\"\"\n">>, D, {ok, [[<<>>, <<>>]]}},
        {whitespace_kept, <<" a , b \n">>, D, {ok, [[<<" a ">>, <<" b ">>]]}},
        {cr_kept_with_lf_delim, <<"a,b\r\n">>, D, {ok, [[<<"a">>, <<"b\r">>]]}},
        {custom_quote, <<"'a''b',c\n">>, #{quotes => <<$'>>}, {ok, [[<<"a'b">>, <<"c">>]]}},
        {custom_separator, <<"a;b\n">>, #{separator => <<$;>>}, {ok, [[<<"a">>, <<"b">>]]}},
        {crlf_delimiter, <<"a,b\r\nc,d\r\n">>, #{delimiter => <<"\r\n">>},
            {ok, [[<<"a">>, <<"b">>], [<<"c">>, <<"d">>]]}},
        {crlf_partial, <<"a,b\r\nc,d">>, #{delimiter => <<"\r\n">>},
            {has_trailer, [[<<"a">>, <<"b">>]], <<"c,d">>}}
    ].

stray_quote_is_lossless(_Config) ->
    % A quote in the middle of an otherwise unquoted field is malformed CSV. The
    % old regex decoder silently dropped the `d"' fragment and returned
    % [[a, e, f]]; the byte-matching decoder keeps every byte instead.
    ?assertEqual(
        {ok, [[<<"a">>, <<"d\"e">>, <<"f">>]]},
        erl_csv:decode(<<"a,d\"e,f\n">>)
    ).

lone_quote_is_a_trailer(_Config) ->
    % A lone opening quote followed by a newline is the start of a quoted field
    % that spans the newline, not an empty field. The old decoder dropped the
    % quote and returned {ok, [[<<>>]]}; treating it as an (incomplete) trailer
    % is what lets multi-line quoted fields reassemble across chunks.
    ?assertEqual({has_trailer, [], <<"\"\n">>}, erl_csv:decode(<<"\"\n">>)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Learning tests ported from nimble_csv
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nimble_basic(_Config) ->
    ?assertEqual(
        {ok, [[<<"name">>, <<"last">>, <<"year">>], [<<"john">>, <<"doe">>, <<"1986">>]]},
        erl_csv:decode(<<"name,last,year\njohn,doe,1986\n">>)
    ).

nimble_without_trailing_newline(_Config) ->
    % nimble drops the trailing newline entirely; erl_csv is a chunk decoder, so
    % the last unterminated line is handed back as a trailer.
    ?assertEqual(
        {has_trailer, [[<<"john">>, <<"doe">>, <<"1986">>]], <<"mary,jane,1985">>},
        erl_csv:decode(<<"john,doe,1986\nmary,jane,1985">>)
    ).

nimble_crlf(_Config) ->
    ?assertEqual(
        {ok, [[<<"name">>, <<"last">>], [<<"john">>, <<"doe">>]]},
        erl_csv:decode(<<"name,last\r\njohn,doe\r\n">>, #{delimiter => <<"\r\n">>})
    ).

nimble_empty_string(_Config) ->
    ?assertEqual({nomatch, <<>>}, erl_csv:decode(<<>>)).

nimble_blank_lines(_Config) ->
    % Blank lines are rows with a single empty field.
    ?assertEqual(
        {ok, [[<<"name">>], [<<>>], [<<"john">>], [<<>>]]},
        erl_csv:decode(<<"name\n\njohn\n\n">>)
    ).

nimble_whitespace(_Config) ->
    ?assertEqual(
        {ok, [[<<" john ">>, <<" doe ">>, <<" 1986 ">>]]},
        erl_csv:decode(<<" john , doe , 1986 \n">>)
    ).

nimble_escapes(_Config) ->
    ?assertEqual(
        {ok, [[<<"john">>, <<"doe">>, <<"1986">>]]},
        erl_csv:decode(<<"\"john\",doe,\"1986\"\n">>)
    ).

nimble_separator_inside_quotes(_Config) ->
    ?assertEqual(
        {ok, [[<<"doe, john">>, <<"1986">>], [<<"jane, mary">>, <<"1985">>]]},
        erl_csv:decode(<<"\"doe, john\",1986\n\"jane, mary\",1985\n">>)
    ).

nimble_multiline_quoted(_Config) ->
    Input = <<
        "john,\"doe\",\"this is a\nreally long comment\nwith multiple lines\"\n"
        "mary,jane,short comment\n"
    >>,
    Expected =
        {ok, [
            [<<"john">>, <<"doe">>, <<"this is a\nreally long comment\nwith multiple lines">>],
            [<<"mary">>, <<"jane">>, <<"short comment">>]
        ]},
    ?assertEqual(Expected, erl_csv:decode(Input)).

nimble_escaped_escapes(_Config) ->
    Input = <<
        "john,doe,\"with \"\"double-quotes\"\" inside\"\n"
        "mary,jane,\"with , inside\"\n"
    >>,
    Expected =
        {ok, [
            [<<"john">>, <<"doe">>, <<"with \"double-quotes\" inside">>],
            [<<"mary">>, <<"jane">>, <<"with , inside">>]
        ]},
    ?assertEqual(Expected, erl_csv:decode(Input)).

nimble_unterminated_quote(_Config) ->
    % nimble raises here; erl_csv treats an unterminated quoted field as an
    % incomplete row to be completed by a later chunk.
    ?assertEqual(
        {has_trailer, [], <<"john,doe,\"1986\n">>},
        erl_csv:decode(<<"john,doe,\"1986\n">>)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Round-trip properties (encode |> decode)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

roundtrip_special_chars(_Config) ->
    Rows = [
        [<<"plain">>, <<"two">>],
        [<<"with,comma">>, <<"x">>],
        [<<"with\"quote">>, <<"y">>],
        [<<"with\nnewline">>, <<"z">>],
        [<<"with\r\ncrlf">>, <<"w">>],
        [<<>>, <<"empty-left">>],
        [<<"empty-right">>, <<>>],
        [<<"q\"\"q">>, <<"doubled">>],
        [<<"caf", 233/utf8, " ", 9731/utf8>>, <<"unicode">>]
    ],
    Encoded = iolist_to_binary(erl_csv:encode(Rows)),
    ?assertEqual({ok, Rows}, erl_csv:decode(Encoded)).

roundtrip_fuzz(_Config) ->
    % Any list of rows of binary fields (>= 1 field per row) must survive an
    % encode |> decode round-trip, whatever bytes the fields contain.
    _ = rand:seed(exsss, {19, 86, 2025}),
    lists:foreach(
        fun(_) ->
            Rows = random_rows(),
            Encoded = iolist_to_binary(erl_csv:encode(Rows)),
            case erl_csv:decode(Encoded) of
                {ok, Rows} ->
                    ok;
                Other ->
                    ct:fail("round-trip failed~nrows: ~p~nencoded: ~p~ngot: ~p", [
                        Rows, Encoded, Other
                    ])
            end
        end,
        lists:seq(1, 500)
    ).

random_rows() ->
    [random_row() || _ <- lists:seq(1, rand:uniform(5))].

random_row() ->
    [random_field() || _ <- lists:seq(1, rand:uniform(4))].

random_field() ->
    list_to_binary([rand:uniform(256) - 1 || _ <- lists:seq(1, rand:uniform(9) - 1)]).
