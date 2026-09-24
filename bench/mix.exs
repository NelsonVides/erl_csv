defmodule ErlCsvBench.MixProject do
  use Mix.Project

  def project do
    [
      app: :erl_csv_bench,
      version: "0.1.0",
      elixir: "~> 1.15",
      deps: deps()
    ]
  end

  defp deps do
    [
      {:erl_csv, path: "..", manager: :rebar3},
      {:nimble_csv, "~> 1.3"},
      {:glazer, "~> 1.1"},
      {:benchee, "~> 1.5"}
    ]
  end
end
