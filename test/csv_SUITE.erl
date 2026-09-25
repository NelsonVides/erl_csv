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
        {group, encode_edge},
        {group, nimble},
        {group, glazer},
        {group, roundtrip},
        {group, regressions}
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
            lone_quote_is_a_trailer,
            decode_split_anywhere,
            default_options_match_generic
        ]},
        {encode_edge, [parallel], [
            encode_table
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
        % Learning tests ported from glazer, adapted to erl_csv's chunk API. How
        % the three libraries differ on edge cases is tracked in
        % bench/conformance.exs.
        {glazer, [parallel], [
            glazer_decode,
            glazer_encode,
            glazer_iolist_input,
            glazer_round_trip
        ]},
        {roundtrip, [parallel], [
            roundtrip_special_chars,
            roundtrip_fuzz
        ]},
        {regressions, [parallel], [
            stream_last_row_without_line_break,
            stream_last_line_without_separator,
            stream_last_row_keeps_options,
            stream_unterminated_quote_at_end,
            stream_missing_file,
            stream_crlf_inside_quotes,
            stream_small_chunks,
            encode_headers_list_writes_header_row,
            encode_headers_true_looks_values_up_by_key,
            encode_term_with_quotes,
            encode_atom_with_separator,
            encode_utf8_atoms_and_terms,
            encode_floats_round_trip
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

decode_split_anywhere(_Config) ->
    % Decoding a chunk and carrying its trailer over into the next one must give
    % the same rows as decoding the whole input at once, wherever it is cut: in
    % the middle of a field, of an escaped quote or of a CRLF delimiter.
    _ = rand:seed(exsss, {5, 4, 3}),
    Failures = lists:flatmap(
        fun(_) ->
            Rows = random_rows(),
            Opts = lists:nth(rand:uniform(2), [#{}, #{delimiter => <<"\r\n">>}]),
            Encoded = iolist_to_binary(erl_csv:encode(Rows, Opts)),
            [
                #{rows => Rows, opts => Opts, cut => Cut, got => Got}
             || Cut <- lists:seq(0, byte_size(Encoded)),
                Got <- [decode_in_two(Encoded, Cut, Opts)],
                Got =/= Rows
            ]
        end,
        lists:seq(1, 200)
    ),
    ?assertEqual([], lists:sublist(Failures, 3)).

default_options_match_generic(_Config) ->
    % With the default separator and quote, and either line break, decode/2
    % uses a tokenizer of its own. Swapping `,` and `;` in a document and
    % decoding it with `;` as the separator runs the generic tokenizer on the
    % same document: both must give the same result, trailers included.
    _ = rand:seed(exsss, {3, 1, 4}),
    Documents = [random_document() || _ <- lists:seq(1, 20000)],
    Failures = [
        #{input => Input, delimiter => Delimiter, default => Default, generic => Generic}
     || Delimiter <- [<<"\n">>, <<"\r\n">>],
        Input <- Documents,
        Default <- [erl_csv:decode(Input, #{delimiter => Delimiter})],
        Generic <- [
            swap_separators(
                erl_csv:decode(swap_separators(Input), #{
                    separator => <<$;>>, delimiter => Delimiter
                })
            )
        ],
        Default =/= Generic
    ],
    ?assertEqual([], lists:sublist(Failures, 3)).

random_document() ->
    Special = <<",;\"\r\n">>,
    <<
        <<
            (case rand:uniform(3) of
                1 -> binary:at(Special, rand:uniform(byte_size(Special)) - 1);
                _ -> $a + rand:uniform(20)
            end)
        >>
     || _ <- lists:seq(1, rand:uniform(60) - 1)
    >>.

swap_separators(Bin) when is_binary(Bin) ->
    <<<<(swap_separator(C))>> || <<C>> <= Bin>>;
swap_separators(List) when is_list(List) ->
    [swap_separators(X) || X <- List];
swap_separators(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(swap_separators(tuple_to_list(Tuple)));
swap_separators(Other) ->
    Other.

swap_separator($,) -> $;;
swap_separator($;) -> $,;
swap_separator(C) -> C.

decode_in_two(Encoded, Cut, Opts) ->
    <<Part1:Cut/binary, Part2/binary>> = Encoded,
    {Rows1, Carry} = decode_part(Part1, Opts),
    {Rows2, <<>>} = decode_part(<<Carry/binary, Part2/binary>>, Opts),
    Rows1 ++ Rows2.

decode_part(Bin, Opts) ->
    case erl_csv:decode(Bin, Opts) of
        {ok, Rows} -> {Rows, <<>>};
        {has_trailer, Rows, Trailer} -> {Rows, Trailer};
        {nomatch, Trailer} -> {[], Trailer}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Table-driven encode edge cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_table(_Config) ->
    Failures = lists:filtermap(
        fun({Name, Input, Opts, Expected}) ->
            Actual = unicode:characters_to_binary(erl_csv:encode(Input, Opts)),
            case Actual =:= Expected of
                true -> false;
                false -> {true, #{case_name => Name, expected => Expected, actual => Actual}}
            end
        end,
        encode_cases()
    ),
    ?assertEqual([], Failures).

encode_cases() ->
    D = #{},
    [
        % {Name, Input, Opts, Expected}
        {plain, [[<<"a">>, <<"b">>]], D, <<"a,b\n">>},
        {empty_input, [], D, <<>>},
        {empty_row, [[]], D, <<"\n">>},
        {empty_cells, [[<<>>, <<>>]], D, <<",\n">>},
        {separator_quoted, [[<<"a,b">>]], D, <<"\"a,b\"\n">>},
        {newline_quoted, [[<<"a\nb">>]], D, <<"\"a\nb\"\n">>},
        {cr_quoted, [[<<"a\rb">>]], D, <<"\"a\rb\"\n">>},
        {quote_escaped, [[<<"a\"b">>]], D, <<"\"a\"\"b\"\n">>},
        {only_quotes, [[<<"\"\"">>]], D, <<"\"\"\"\"\"\"\n">>},
        {quotes_at_edges, [[<<"\"a\"">>]], D, <<"\"\"\"a\"\"\"\n">>},
        {custom_separator, [[<<"a,b">>, <<"c;d">>]], #{separator => <<$;>>}, <<"a,b;\"c;d\"\n">>},
        {multibyte_delimiter, [[<<"a|b">>, <<"c||d">>]], #{delimiter => <<"||">>},
            <<"a|b,\"c||d\"||">>},
        {crlf_delimiter, [[<<"a">>], [<<"b">>]], #{delimiter => <<"\r\n">>}, <<"a\r\nb\r\n">>},
        {tuple_row, [{<<"a">>, <<"b">>}], D, <<"a,b\n">>},
        {numbers, [[1, -20, 1.5]], D, <<"1,-20,1.5\n">>},
        {atoms, [[true, 'Hello World']], D, <<"true,'Hello World'\n">>},
        {charlists, [["abc", "x,y"]], D, <<"abc,\"x,y\"\n">>},
        {unicode, [[<<"caf", 233/utf8>>, [9731]]], D, <<"caf", 233/utf8, ",", 9731/utf8, "\n">>},
        {tuple_cell, [[{a, 1}]], D, <<"\"{a,1}\"\n">>},
        {headers_true, [#{<<"a">> => 1, <<"b">> => <<"x,y">>}], #{headers => true},
            <<"a,b\n1,\"x,y\"\n">>}
    ].

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
%% Learning tests ported from glazer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

glazer_decode(_Config) ->
    % glazer decodes whole documents; every input here ends with its line break
    % so that erl_csv returns complete rows instead of a trailer.
    Cases = [
        {<<"a,,c\n">>, #{}, [[<<"a">>, <<>>, <<"c">>]]},
        {<<"\"hello, world\",b\n">>, #{}, [[<<"hello, world">>, <<"b">>]]},
        {<<"\"a \"\"quoted\"\" word\"\n">>, #{}, [[<<"a \"quoted\" word">>]]},
        {<<"\"line1\r\nline2\",b\r\n">>, #{delimiter => <<"\r\n">>}, [
            [<<"line1\r\nline2">>, <<"b">>]
        ]},
        {<<"\"\",b\n">>, #{}, [[<<>>, <<"b">>]]},
        {<<"\"\"\"\"\"\"\n">>, #{}, [[<<"\"\"">>]]},
        {<<"a,\"b,c\",d\n">>, #{}, [[<<"a">>, <<"b,c">>, <<"d">>]]},
        {<<"a\tb\n">>, #{separator => <<$\t>>}, [[<<"a">>, <<"b">>]]},
        {<<"\"a;b\";c\n">>, #{separator => <<$;>>}, [[<<"a;b">>, <<"c">>]]},
        {<<";;\n">>, #{separator => <<$;>>}, [[<<>>, <<>>, <<>>]]}
    ],
    Failures = [
        #{input => In, expected => Rows, actual => Actual}
     || {In, Opts, Rows} <- Cases,
        Actual <- [erl_csv:decode(In, Opts)],
        Actual =/= {ok, Rows}
    ],
    ?assertEqual([], Failures).

glazer_encode(_Config) ->
    Cases = [
        {[[<<"a">>, <<"b">>], [1, 2]], #{}, <<"a,b\n1,2\n">>},
        {[[<<"a">>, <<"b">>], [1, 2]], #{delimiter => <<"\r\n">>}, <<"a,b\r\n1,2\r\n">>},
        {[[]], #{}, <<"\n">>},
        {[[123456789012345678901234567890]], #{}, <<"123456789012345678901234567890\n">>},
        {[[-1, 2]], #{}, <<"-1,2\n">>},
        {[[<<"hello, world">>, <<"b">>]], #{}, <<"\"hello, world\",b\n">>},
        {[[<<"a \"quoted\" word">>]], #{}, <<"\"a \"\"quoted\"\" word\"\n">>},
        {[[<<"line1\nline2">>]], #{}, <<"\"line1\nline2\"\n">>},
        {[[<<"a">>, <<"b">>]], #{separator => <<$;>>}, <<"a;b\n">>},
        {[[foo, 1.5]], #{}, <<"foo,1.5\n">>},
        {[#{<<"a">> => 1, <<"b">> => 2}], #{headers => true}, <<"a,b\n1,2\n">>},
        {[#{<<"a">> => 1, <<"b">> => 2}], #{headers => [<<"b">>, <<"a">>]}, <<"b,a\n2,1\n">>}
    ],
    Failures = [
        #{input => Rows, expected => Csv, actual => Actual}
     || {Rows, Opts, Csv} <- Cases,
        Actual <- [iolist_to_binary(erl_csv:encode(Rows, Opts))],
        Actual =/= Csv
    ],
    ?assertEqual([], Failures).

glazer_iolist_input(_Config) ->
    % Input split into nested iodata, even in the middle of a field or of an
    % escaped quote, decodes as if it were one binary.
    ?assertEqual(
        {ok, [[<<"a">>, <<"b">>, <<"c">>], [<<"1">>, <<"2">>, <<"3">>]]},
        erl_csv:decode([<<"a">>, [<<",">>, <<"b">>], [[<<",c\n">>]], <<"1,2,3\n">>])
    ),
    ?assertEqual(
        {ok, [[<<"a">>, <<"b\"c">>, <<"d">>]]},
        erl_csv:decode([<<"a,\"b\"\"">>, <<"c\",d\n">>])
    ).

glazer_round_trip(_Config) ->
    Rows = [[<<"a \"quoted\"\nvalue">>, <<"b">>]],
    ?assertEqual({ok, Rows}, erl_csv:decode(iolist_to_binary(erl_csv:encode(Rows)))),
    Opts = #{headers => true, separator => <<$;>>},
    Encoded = iolist_to_binary(erl_csv:encode([#{<<"a">> => <<"1">>, <<"b">> => <<"x;y">>}], Opts)),
    ?assertEqual(
        {ok, [[<<"a">>, <<"b">>], [<<"1">>, <<"x;y">>]]},
        erl_csv:decode(Encoded, #{separator => <<$;>>})
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Regressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stream_last_row_without_line_break(Config) ->
    % The last row used to be dropped when the file did not end with a line
    % break: it was waiting as a trailer when the stream ran out.
    File = write_file(Config, "last_row.csv", <<"a,b\n1,2\n3,4">>),
    ?assertEqual(
        {[[<<"a">>, <<"b">>], [<<"1">>, <<"2">>], [<<"3">>, <<"4">>]], stream_end},
        stream_file(File, #{})
    ).

stream_last_line_without_separator(Config) ->
    % Same, for a last line with neither a separator nor a line break.
    File = write_file(Config, "last_line.csv", <<"a\nb">>),
    ?assertEqual({[[<<"a">>], [<<"b">>]], stream_end}, stream_file(File, #{})).

stream_last_row_keeps_options(Config) ->
    % The options given to decode_new_s/2 used to be ignored altogether.
    File = write_file(Config, "last_row_opts.csv", <<"a;b\r\n1;2">>),
    Opts = #{separator => <<$;>>, delimiter => <<"\r\n">>},
    ?assertEqual({[[<<"a">>, <<"b">>], [<<"1">>, <<"2">>]], stream_end}, stream_file(File, Opts)).

stream_unterminated_quote_at_end(Config) ->
    % A quoted field that is never closed used to be dropped silently too.
    File = write_file(Config, "unterminated.csv", <<"a,b\n\"c,d\n">>),
    ?assertEqual(
        {[[<<"a">>, <<"b">>]], {error, {unterminated_quoted_field, <<"\"c,d\n">>}}},
        stream_file(File, #{})
    ).

stream_missing_file(Config) ->
    % Used to crash with a badmatch instead of returning the error.
    File = filename:join(?config(priv_dir, Config), "does_not_exist.csv"),
    ?assertEqual({error, enoent}, erl_csv:decode_new_s(File)).

stream_crlf_inside_quotes(Config) ->
    % Files used to be read line by line, which turned every CRLF into LF, even
    % inside quoted fields.
    File = write_file(Config, "crlf.csv", <<"a,\"x\r\ny\"\r\nb,c\r\n">>),
    ?assertEqual(
        {[[<<"a">>, <<"x\r\ny">>], [<<"b">>, <<"c">>]], stream_end},
        stream_file(File, #{delimiter => <<"\r\n">>})
    ).

stream_small_chunks(Config) ->
    % Tiny reads cut rows, quoted fields, escaped quotes and CRLF delimiters at
    % every possible place; the stream must still match decoding the whole file.
    _ = rand:seed(exsss, {9, 8, 7}),
    Opts = #{delimiter => <<"\r\n">>},
    Rows = lists:append([random_rows() || _ <- lists:seq(1, 20)]),
    Encoded = iolist_to_binary(erl_csv:encode(Rows, Opts)),
    File = write_file(Config, "chunks.csv", Encoded),
    ?assertEqual({ok, Rows}, erl_csv:decode(Encoded, Opts)),
    [
        ?assertEqual({Rows, stream_end}, stream_file(File, Opts#{iobuf => Size}))
     || Size <- [1, 2, 3, 7, 64]
    ].

encode_headers_list_writes_header_row(_Config) ->
    % The header row was documented but never written for a list of headers.
    Rows = [#{a => 1, b => 2}, #{a => 3, b => 4}],
    ?assertEqual(
        <<"b,a\n2,1\n4,3\n">>,
        iolist_to_binary(erl_csv:encode(Rows, #{headers => [b, a]}))
    ).

encode_headers_true_looks_values_up_by_key(_Config) ->
    % Values used to be taken in each map's own order, so a map with an extra
    % key shifted its values under the wrong headers.
    % Binary keys, since maps:keys/1 orders atom keys by their place in the
    % atom table.
    Rows = [#{<<"a">> => 1, <<"b">> => 2}, #{<<"a">> => 3, <<"b">> => 4, <<"c">> => 5}],
    ?assertEqual(
        <<"a,b\n1,2\n3,4\n">>,
        iolist_to_binary(erl_csv:encode(Rows, #{headers => true}))
    ),
    ?assertError(
        {badkey, <<"b">>},
        erl_csv:encode([#{<<"a">> => 1, <<"b">> => 2}, #{<<"a">> => 3}], #{headers => true})
    ).

encode_term_with_quotes(_Config) ->
    % Tuples and other terms were quoted without doubling the quotes inside,
    % which ended the field early.
    Encoded = iolist_to_binary(erl_csv:encode([[{a, "b"}, <<"c">>]])),
    ?assertEqual(<<"\"{a,\"\"b\"\"}\",c\n">>, Encoded),
    ?assertEqual({ok, [[<<"{a,\"b\"}">>, <<"c">>]]}, erl_csv:decode(Encoded)).

encode_atom_with_separator(_Config) ->
    ?assertEqual(<<"\"'a,b'\",c\n">>, iolist_to_binary(erl_csv:encode([['a,b', c]]))).

encode_utf8_atoms_and_terms(_Config) ->
    % Atoms and terms were written as code points rather than UTF-8, so
    % iolist_to_binary/1 either crashed or produced Latin-1. The term prints the
    % same whatever the VM's printable range (+pc): a binary or list with
    % characters beyond Latin-1 would not.
    Rows = [[café, '日本', {'é', '日本', <<"é"/utf8>>}]],
    ?assertEqual(
        <<"café,'日本',\"{é,'日本',<<\"\"é\"\"/utf8>>}\"\n"/utf8>>,
        iolist_to_binary(erl_csv:encode(Rows))
    ).

encode_floats_round_trip(_Config) ->
    % Floats were written with six decimals, so 1.0e-7 became 0.000000.
    Floats = [1.5, 1.0e-7, 0.1, -0.0, 1.0e20, 123456789.123],
    Encoded = iolist_to_binary(erl_csv:encode([Floats])),
    ?assertEqual(<<"1.5,1.0e-7,0.1,-0.0,1.0e20,123456789.123\n">>, Encoded),
    {ok, [Decoded]} = erl_csv:decode(Encoded),
    ?assertEqual(Floats, [binary_to_float(F) || F <- Decoded]).

write_file(Config, Name, Content) ->
    File = filename:join(?config(priv_dir, Config), Name),
    ok = file:write_file(File, Content),
    File.

%% Consume a whole stream: the rows, and how it ended.
stream_file(File, Opts) ->
    {ok, Stream} = erl_csv:decode_new_s(File, Opts),
    stream_rows(Stream, []).

stream_rows(stream_end, Acc) ->
    {lists:append(lists:reverse(Acc)), stream_end};
stream_rows(Stream, Acc) ->
    case erl_csv:decode_s(Stream) of
        {ok, Rows, Next} -> stream_rows(Next, [Rows | Acc]);
        {error, Reason} -> {lists:append(lists:reverse(Acc)), {error, Reason}}
    end.
