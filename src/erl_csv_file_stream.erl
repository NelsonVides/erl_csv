%% @private
%%   Copyright 2022 Nelson Vides, All Rights Reserved
%%   Copyright 2012 - 2014 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
-module(erl_csv_file_stream).
-compile({no_auto_import, [hd/1, tl/1]}).

-include("erl_csv.hrl").

-export([hd/1, tl/1, foreach/2, map/2, list/1, list/2]).
-export([new/0, new/1, new/2, read_file/2]).

-define(DEFAULT_BUFFER_SIZE, 64 * 1024).

-spec hd(csv_stream()) -> iodata().
hd(#csv_stream{hd = Head}) ->
    Head;
hd(stream_end) ->
    <<>>.

-spec tl(csv_stream()) -> maybe_csv_stream().
tl(#csv_stream{hd = <<>>, tl = Tail}) ->
    Tail();
tl(#csv_stream{hd = Head, tl = Tail}) ->
    case Tail() of
        stream_end ->
            stream_end;
        #csv_stream{hd = MoreHead} = NewStream ->
            NewStream#csv_stream{hd = iolist_to_binary([Head, MoreHead])};
        {error, Reason} ->
            {error, Reason}
    end;
tl(stream_end) ->
    stream_end.

-spec foreach(fun((term()) -> term()), maybe_csv_stream()) -> stream_end | {error, term()}.
foreach(_Fun, stream_end) ->
    stream_end;
foreach(Fun, #csv_stream{hd = Head} = Stream) ->
    Fun(Head),
    foreach(Fun, tl(Stream));
foreach(_, {error, Reason}) ->
    {error, Reason}.

-spec map(fun((T1) -> T2), maybe_csv_stream()) ->
    [T2 | {error, term()}] when
      T1 :: term(),
      T2 :: term().
map(Fun, stream_end) when is_function(Fun, 1) ->
    [];
map(Fun, #csv_stream{hd = Head} = Stream) ->
    [ Fun(Head) | map(Fun, tl(Stream)) ];
map(_, {error, Reason}) ->
    [{error, Reason}].

-spec list(csv_stream()) -> list().
list(#csv_stream{hd = Head} = Stream) ->
    [ Head | list(tl(Stream))];
list(_) ->
    [].

-spec list(integer(), csv_stream()) -> list().
list(N, #csv_stream{hd = Head} = Stream) when N > 0 ->
    [ Head | list(N - 1, tl(Stream))];
list(_, _) ->
    [].

%% create file csv_stream
%%  Options:
%%    * {iobuf, integer()} - size of i/o buffer
-spec new() -> csv_stream().
new() ->
    stream_end.

-spec new(binary()) -> csv_stream().
new(Head) ->
    new(Head, fun new/0).

-spec new(iodata(), csv_stream_fun()) -> csv_stream().
new(Head, Fun) when is_function(Fun, 0) ->
    #csv_stream{hd = Head, tl = Fun}.

-spec read_file(file:name_all(), map()) -> maybe_csv_stream().
read_file(File, Opts) ->
    BufferSize = maps:get(iobuf, Opts, ?DEFAULT_BUFFER_SIZE),
    {ok, FD} = file:open(File, [raw, binary, read, {read_ahead, BufferSize}]),
    istream(FD).

-spec istream(term()) -> maybe_csv_stream().
istream(FD) when is_tuple(FD), erlang:element(1, FD) =:= file_descriptor ->
    case file:read_line(FD) of
        {ok, Chunk} ->
            new(Chunk, fun() -> istream(FD) end);
        eof  ->
            file:close(FD),
            new();
        {error, Reason} ->
            file:close(FD),
            {error, Reason}
    end.

% -spec write_file(file:name(), list(), csv_stream()) -> ok | {error, any()}.
% write_file(File, Opts, Stream) ->
%     Chunk = proplists:get_value(iobuf, Opts, ?DEFAULT_BUFFER_SIZE),
%     {ok, FD} = file:open(File, [raw, binary, append, {delayed_write, Chunk, 5000}]),
%     ostream(FD, Stream).

% ostream(FD, #csv_stream{} = Stream) when is_tuple(FD), erlang:element(1, FD) =:= file_descriptor ->
%     case file:write(FD, hd(Stream)) of
%         ok ->
%             ostream(FD, tl(Stream));
%         Error ->
%             file:close(FD),
%             Error
%     end;
% ostream(FD, {}) when is_tuple(FD), erlang:element(1, FD) =:= file_descriptor ->
%     file:close(FD).
