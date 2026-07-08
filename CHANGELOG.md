# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## main

## v0.5.0

### Changed

- Rewrote the decoder as a binary-matching tokenizer instead of a regular
  expression, following the approach in `nimble_csv`. Typical data decodes
  around 5x faster, and un-escaping only runs when a quoted field actually
  contains a doubled quote.

### Fixed

- Malformed input is no longer silently truncated while decoding: a stray quote
  inside an unquoted field is kept verbatim, and a lone opening quote at the end
  of a chunk is treated as the start of an unterminated (multi-line) quoted
  field rather than as an empty field.

### Removed

- The undocumented `regex` decode option, which the decoder no longer uses.

## v0.4.0

### Added

- `erlfmt` and `rebar3_lint`, wired into a `rebar3 check` alias and run in CI.
- Dependabot configuration to keep the GitHub Actions up to date.
- A release workflow that publishes the package and documentation to Hex and
  cuts a GitHub release on `v*.*.*` tags.
- A manual `workflow_dispatch` trigger for the CI workflow.

### Changed

- The CI matrix now runs on the last three OTP releases (27, 28 and 29), and
  `minimum_otp_vsn` is set to `27`.
- Documentation is written with native `-moduledoc`/`-doc` attributes instead of
  EDoc `@doc` comments.
- Encoding is substantially faster: the set of characters that force a field to
  be quoted is compiled once per call and matched with `binary:match/2` instead
  of recompiling a regular expression for every cell.
- Bumped the GitHub Actions (`checkout` v7, `cache` v6) and moved the runner to
  `ubuntu-26.04`; pinned the `rebar3_hex` and `rebar3_ex_doc` plugin versions.

### Fixed

- The `quotes` decode option was silently ignored because of a misspelled key,
  and is now honoured.
- Decoding with a multi-byte delimiter such as `<<"\r\n">>` no longer crashes
  with a `case_clause`.
- Repaired the README badges (the broken Hex Docs badge and the plain-`http`
  Hex version badge).

## 0.3.3

### Changed

- Moved the public types out of the include file and into the `erl_csv` module.
- Updated the GitHub Actions workflow.

## 0.3.2

### Added

- Published documentation to HexDocs through the `ex_doc` provider.

## 0.3.1

### Fixed

- Exported the `csv_stream_fun/0` type.

## 0.3.0

### Added

- Continuous integration on GitHub Actions.

### Changed

- Decoders accept and return `iodata` where applicable, and the file-decoder
  file-name type was made consistent.

### Fixed

- No longer raises an exception when a chunk ends with a trailer (an incomplete
  trailing line).

## 0.2.0

### Added

- Streaming file decoder (`decode_new_s/1,2` and `decode_s/1`).
- Encoding of records and tuples.

### Fixed

- Decoding type specifications and Dialyzer warnings.

## 0.1.0

### Added

- Initial release: RFC 4180 compliant CSV encoding and decoding.
