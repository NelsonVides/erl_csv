differing = ErlCsvBench.Conformance.run()

if differing != [] do
  IO.puts("\n#{length(differing)} case(s) where erl_csv departs from nimble_csv and glazer")
  System.halt(1)
end
