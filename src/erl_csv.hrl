-ifndef(ERL_CSV).
-define(ERL_CSV, true).

-define(SEPARATOR, <<$,>>).
-define(NEWLINE, <<$\n>>).
-define(QUOTES, <<$">>).
-define(CARRIAGE_RETURN, <<$\r>>).
-define(DELIMITER, <<$\n>>).

-record(csv_stream, {
          hd = <<>> :: iodata(),
          tl = fun() -> stream_end end :: erl_csv:csv_stream_fun(),
          opts = #{} :: erl_csv:decode_opts()
         }).

-type csv_stream() :: #csv_stream{} | stream_end.
-type maybe_csv_stream() :: csv_stream() | {error, term()}.
-type csv_stream_fun() :: fun(() -> maybe_csv_stream()).
-export_type([csv_stream/0, csv_stream_fun/0]).

-endif.
