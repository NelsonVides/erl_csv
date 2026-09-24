# Like NimbleCSV.RFC4180, which writes CRLF line breaks, but with other separators.
NimbleCSV.define(ErlCsvBench.Nimble.Semicolon,
  separator: ";",
  escape: "\"",
  line_separator: "\r\n"
)

NimbleCSV.define(ErlCsvBench.Nimble.Tab, separator: "\t", escape: "\"", line_separator: "\r\n")

defmodule ErlCsvBench.Conformance do
  @moduledoc """
  Runs the same small CSV documents (and rows to encode) through erl_csv,
  nimble_csv and glazer, and compares the results.

  nimble_csv and glazer are independent implementations, so where they agree
  their answer is taken as the expected one. A case where erl_csv returns
  something else is a failure, unless it is listed in `@known_differences`
  with the reason erl_csv deliberately behaves differently. Where nimble_csv and
  glazer disagree with each other there is no consensus, and the case is only
  reported.
  """

  # {name, input, separator, erl_csv line delimiter}
  @decode_cases [
    # From glazer's test suite
    {"rows", "a,b\n1,2\n", ",", "\n"},
    {"CRLF rows", "a,b\r\n1,2\r\n", ",", "\r\n"},
    {"empty fields", "a,,c\n", ",", "\n"},
    {"blank line", "a\n\nb\n", ",", "\n"},
    {"quoted separator", "\"hello, world\",b\n", ",", "\n"},
    {"doubled quotes", "\"a \"\"quoted\"\" word\"\n", ",", "\n"},
    {"LF inside quotes", "\"line1\nline2\",b\n", ",", "\n"},
    {"CRLF inside quotes", "\"line1\r\nline2\",b\r\n", ",", "\r\n"},
    {"empty quoted field", "\"\",b\n", ",", "\n"},
    {"whole row quoted", "\"a,b\"\n", ",", "\n"},
    {"only doubled quotes", "\"\"\"\"\"\"\n", ",", "\n"},
    {"quoted between unquoted", "a,\"b,c\",d\n", ",", "\n"},
    {"semicolon separator", "a;b;c\n", ";", "\n"},
    {"tab separator", "a\tb\n", "\t", "\n"},
    {"quoted custom separator", "\"a;b\";c\n", ";", "\n"},
    {"only separators", ";;\n", ";", "\n"},
    {"no final line break", "a,b\n1,2", ",", "\n"},
    {"unterminated quote", "\"unterminated\n", ",", "\n"},
    # From nimble_csv's test suite
    {"whitespace kept", " john , doe \n", ",", "\n"},
    {"multi-line quoted",
     "john,\"this is a\nreally long comment\nwith multiple lines\"\nmary,jane\n", ",", "\n"},
    # Edge cases of our own
    {"empty input", "", ",", "\n"},
    {"single line break", "\n", ",", "\n"},
    {"trailing separator", "a,\n", ",", "\n"},
    {"UTF-8", "café,☃\n", ",", "\n"},
    {"stray quote in unquoted field", "a,d\"e,f\n", ",", "\n"},
    {"text after closing quote", "\"a\"b,c\n", ",", "\n"},
    {"space before opening quote", "a, \"b\"\n", ",", "\n"},
    {"CR inside a field", "a\rb,c\n", ",", "\n"},
    {"CR inside a field, CRLF rows", "a\rb,c\r\n", ",", "\r\n"},
    {"CRLF rows read as LF", "a,b\r\n", ",", "\n"}
  ]

  # {name, rows, separator}; every library writes CRLF line breaks.
  @encode_cases [
    {"rows", [["a", "b"], ["1", "2"]], ","},
    {"separator in field", [["hello, world", "b"]], ","},
    {"quotes in field", [["a \"quoted\" word"]], ","},
    {"LF in field", [["line1\nline2"]], ","},
    {"CR in field", [["a\rb"]], ","},
    {"empty row", [[]], ","},
    {"empty fields", [["", ""]], ","},
    {"spaces kept unquoted", [[" a ", "b "]], ","},
    {"UTF-8", [["café", "☃"]], ","},
    {"semicolon separator", [["a;b", "c,d"]], ";"},
    {"integers", [[1, -2, 123_456_789_012_345_678_901_234_567_890]], ","},
    {"float", [[1.5]], ","},
    {"small float", [[1.0e-7]], ","},
    {"atoms", [[:foo, true]], ","}
  ]

  @chunked "erl_csv:decode/2 works on chunks: a row is only complete once its line " <>
             "break is seen, so it comes back as a trailer to be completed by the next chunk"

  @known_differences %{
    "no final line break" => @chunked,
    "unterminated quote" => @chunked,
    "CRLF rows read as LF" =>
      "erl_csv only splits rows on the configured delimiter, \"\\n\" by default; " <>
        "CRLF input needs delimiter => <<\"\\r\\n\">>"
  }

  def run do
    decode = Enum.map(@decode_cases, &check_decode/1)
    encode = Enum.map(@encode_cases, &check_encode/1)
    report("decode", decode)
    report("encode", encode)
    Enum.filter(decode ++ encode, &match?(%{verdict: :differs}, &1))
  end

  defp check_decode({name, input, sep, delimiter}) do
    results = %{
      erl_csv: erl_csv_decode(input, sep, delimiter),
      nimble_csv: guard(fn -> nimble(sep).parse_string(input, skip_headers: false) end),
      glazer: glazer_decode(input, sep)
    }

    verdict(name, input, results)
  end

  defp check_encode({name, rows, sep}) do
    results = %{
      erl_csv:
        guard(fn ->
          :erl_csv.encode(rows, %{separator: sep, delimiter: "\r\n"})
          |> :unicode.characters_to_binary()
        end),
      nimble_csv: guard(fn -> nimble(sep).dump_to_iodata(rows) |> IO.iodata_to_binary() end),
      glazer: guard(fn -> :glazer_csv.encode(rows, delimiter: :binary.first(sep)) end)
    }

    verdict(name, rows, results)
  end

  defp erl_csv_decode(input, sep, delimiter) do
    case :erl_csv.decode(input, %{separator: sep, delimiter: delimiter}) do
      {:ok, rows} -> {:ok, rows}
      {:nomatch, ""} -> {:ok, []}
      {:nomatch, trailer} -> {:incomplete, [], trailer}
      {:has_trailer, rows, trailer} -> {:incomplete, rows, trailer}
    end
  end

  defp glazer_decode(input, sep) do
    case :glazer_csv.try_decode(input, delimiter: :binary.first(sep)) do
      {:ok, %{data: rows}} -> {:ok, rows}
      {:error, _} -> :error
    end
  end

  defp nimble(","), do: NimbleCSV.RFC4180
  defp nimble(";"), do: ErlCsvBench.Nimble.Semicolon
  defp nimble("\t"), do: ErlCsvBench.Nimble.Tab

  defp guard(fun) do
    {:ok, fun.()}
  rescue
    _ -> :error
  catch
    _, _ -> :error
  end

  defp verdict(name, input, %{erl_csv: e, nimble_csv: n, glazer: g} = results) do
    verdict =
      cond do
        n != g -> :no_consensus
        e == g -> :agree
        Map.has_key?(@known_differences, name) -> :known
        true -> :differs
      end

    %{name: name, input: input, results: results, verdict: verdict}
  end

  defp report(kind, checks) do
    counts = Enum.frequencies_by(checks, & &1.verdict)
    IO.puts("\n#{kind}: #{inspect(counts)}")

    for %{verdict: v} = check <- checks, v != :agree do
      IO.puts("  [#{v}] #{check.name}: #{inspect(check.input)}")
      if v == :known, do: IO.puts("      why: #{@known_differences[check.name]}")

      for {lib, result} <- Enum.sort(check.results) do
        IO.puts("      #{String.pad_trailing(to_string(lib), 10)} #{inspect(result)}")
      end
    end
  end
end
