# erl_csv benchmarks

[Benchee](https://hex.pm/packages/benchee) benchmarks of `erl_csv` against
[nimble_csv](https://hex.pm/packages/nimble_csv) (pure Elixir) and
[glazer](https://hex.pm/packages/glazer) (a C++ NIF), for both decoding
and encoding.

This directory is a standalone Mix project that depends on the `erl_csv` in the
parent directory. It is not part of the Hex package.

## Running

You need Elixir and a C++ compiler that can build glazer's NIF.

```sh
cd bench
mix deps.get
mix run decode.exs
mix run encode.exs
mix run conformance.exs
```

Before measuring anything, the decode and encode scripts check that the three
libraries produce the same result for every input, and fail if they don't.

## Conformance

`conformance.exs` runs small CSV documents (and rows to encode) through all three
libraries: cases from glazer's and nimble_csv's test suites, plus edge cases of
our own. nimble_csv and glazer are independent implementations, so where they
agree their answer is taken as the expected one, and the script fails if
`erl_csv` returns something else. The exceptions are deliberate differences, each
listed with its reason in `lib/erl_csv_bench/conformance.ex`, such as the
trailer `erl_csv:decode/2` returns for a row without a final line break. Where
nimble_csv and glazer disagree with each other, the script only reports what
each library returns.

## Input

`data/` holds the `small`, `medium` and `large` files from glazer's own CSV
benchmark ([saleyn/glazer@87bf560](https://github.com/saleyn/glazer/tree/87bf5605d5f1986c1b9bed8a98467224309ef51e/test/data)),
copied unchanged under glazer's MIT license (`data/LICENSE.glazer`). Every row has
six fields: two are quoted because they contain a comma, and one of those also
contains escaped (doubled) quotes.

The files use CRLF line endings, so `erl_csv` is called with
`#{delimiter => <<"\r\n">>}`. With the default `"\n"` delimiter it would return
the wrong rows: the last field would keep its quotes and a trailing `"\r"`.
nimble_csv's `RFC4180` parser and glazer accept both line endings.

- **Decode:** the CSV binary to a list of rows of binaries.
- **Encode:** the rows `erl_csv` decodes from each file back to a CSV binary, with
  CRLF line endings for all three libraries. `erl_csv` and nimble_csv return
  iodata, which is flattened with `IO.iodata_to_binary/1` so that every library
  produces the same binary.

## Reading the results

Benchee runs one job at a time, after a warm-up, and reports the distribution of
run times. The memory figures only count what the benchmarked process allocates
from Erlang code: the terms glazer's NIF builds are not included, so its memory
column is not comparable with the others.
