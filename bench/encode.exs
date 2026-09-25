# Every library encodes the same rows: the ones erl_csv decodes from each file.
decode = ErlCsvBench.decoders()["erl_csv"]
inputs = for {name, input} <- ErlCsvBench.inputs([:crlf]), do: {name, decode.(input)}
encoders = ErlCsvBench.encoders()

ErlCsvBench.check_agreement!(encoders, inputs)
Benchee.run(encoders, ErlCsvBench.benchee_options(inputs))
