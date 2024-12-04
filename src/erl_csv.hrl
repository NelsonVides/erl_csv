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

-endif.
