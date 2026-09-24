# Every library encodes the same rows: the ones erl_csv decodes from each file.
decode = ErlCsvBench.decoders()["erl_csv"]
inputs = for {name, bin} <- ErlCsvBench.inputs(), do: {name, decode.(bin)}
encoders = ErlCsvBench.encoders()

ErlCsvBench.check_agreement!(encoders, inputs)
Benchee.run(encoders, ErlCsvBench.benchee_options(inputs))
