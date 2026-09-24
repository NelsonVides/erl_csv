inputs = ErlCsvBench.inputs()
decoders = ErlCsvBench.decoders()

ErlCsvBench.check_agreement!(decoders, inputs)
Benchee.run(decoders, ErlCsvBench.benchee_options(inputs))
