-module(csv_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, encode},
     {group, decode}
    ].

groups() ->
    [
     {encode, [parallel],
      [
       quotes_and_newlines,
       escaped_quotes,
       utf8,
       json
      ]
     },
     {decode, [parallel],
      [
       quotes_and_newlines,
       escaped_quotes,
       utf8,
       json
      ]
     }
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

quotes_and_newlines(Config) ->
    % given
    Decoded = [[<<"a">>, <<"b">>],
               [<<"1">>, <<"ha, \n\"ha\", \nha">>],
               [<<"3">>, <<"4">>]],
    Encoded = <<"a,b\n1,\"ha, \n\"\"ha\"\", \nha\"\n3,4\n">>,
    % then
    run_test(which_group(Config), Encoded, Decoded).

escaped_quotes(Config) ->
    % given
    Decoded = [[<<"a">>, <<"b">>],
               [<<"1">>, <<"ha \"ha\" ha">>],
               [<<"3">>, <<"4">>]],
    Encoded = <<"a,b\n1,\"ha \"\"ha\"\" ha\"\n3,4\n">>,
    % when
    run_test(which_group(Config), Encoded, Decoded).

utf8(Config) ->
    % given
    Decoded = [[<<"a">>,<<13371/utf8>>,<<"c">>],
               [<<"1">>,<<"2">>,<<"3">>],
               [<<"4">>,<<"5">>,<<676/utf8>>]],
    Encoded = <<97,44,227,144,187,44,99,10,49,44,50,44,51,10,52,44,53,44,202,164,10>>,
    % when
    run_test(which_group(Config), Encoded, Decoded).

json(Config) ->
    % given
    Decoded = [[<<"key">>, <<"val">>],
               [<<"1">>, <<"{\"type\": \"Point\",\"coordinates\": [102.0, 0.5]}">>]],
    Encoded = <<"key,val\n1,\"{\"\"type\"\": \"\"Point\"\",\"\"coordinates\"\": [102.0, 0.5]}\"\n">>,
    % when
    run_test(which_group(Config), Encoded, Decoded).
