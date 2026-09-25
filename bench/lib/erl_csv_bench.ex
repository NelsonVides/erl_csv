defmodule ErlCsvBench do
  @moduledoc """
  Shared setup for the decode and encode benchmarks: the input files and one
  function per library, so both scripts compare exactly the same things.
  """

  @data_dir Path.expand("../data", __DIR__)

  # The input files are glazer's benchmark data, which uses CRLF line endings.
  # erl_csv defaults to "\n" and must be told; nimble_csv's RFC4180 parser and
  # glazer accept both.
  @erl_csv_opts %{delimiter: "\r\n"}

  @doc """
  The input files, smallest first, labelled with their line breaks and size.
  Each file is used as it is, with CRLF line breaks, and, when `line_breaks`
  includes `:lf`, as a copy with LF line breaks, for which erl_csv gets its
  default options. An input is the CSV binary and the options erl_csv needs.
  """
  def inputs(line_breaks \\ [:crlf, :lf]) do
    for name <- ~w(small medium large), line_break <- line_breaks do
      crlf = File.read!(Path.join(@data_dir, "#{name}.csv"))

      {bin, opts} =
        case line_break do
          :crlf -> {crlf, @erl_csv_opts}
          :lf -> {String.replace(crlf, "\r\n", "\n"), %{}}
        end

      kb = Float.round(byte_size(bin) / 1024, 1)
      {"#{name}, #{String.upcase(to_string(line_break))} (#{kb} KB)", {bin, opts}}
    end
  end

  @doc "CSV binary -> list of rows of binaries."
  def decoders do
    %{
      "erl_csv" => fn {bin, opts} ->
        {:ok, rows} = :erl_csv.decode(bin, opts)
        rows
      end,
      "nimble_csv" => fn {bin, _} ->
        NimbleCSV.RFC4180.parse_string(bin, skip_headers: false)
      end,
      "glazer (NIF)" => fn {bin, _} -> :glazer_csv.decode(bin).data end
    }
  end

  @doc "List of rows of binaries -> CSV binary (CRLF line endings for all)."
  def encoders do
    %{
      "erl_csv" => fn rows -> IO.iodata_to_binary(:erl_csv.encode(rows, @erl_csv_opts)) end,
      "nimble_csv" => fn rows ->
        IO.iodata_to_binary(NimbleCSV.RFC4180.dump_to_iodata(rows))
      end,
      "glazer (NIF)" => fn rows -> :glazer_csv.encode(rows) end
    }
  end

  @doc """
  Fail loudly unless every library returns the same result for every input: a
  benchmark of a library that is not producing the right output (for instance
  because it was given the wrong options) measures nothing useful.
  """
  def check_agreement!(jobs, inputs) do
    for {input_name, input} <- inputs do
      [{reference_name, reference} | others] =
        Enum.map(jobs, fn {job_name, fun} -> {job_name, fun.(input)} end)

      for {job_name, result} <- others, result != reference do
        raise "#{job_name} and #{reference_name} disagree on #{input_name}"
      end
    end

    :ok
  end

  def benchee_options(inputs) do
    [inputs: inputs, warmup: 2, time: 5, memory_time: 1]
  end
end
