defmodule ExUnit.KoanFormatter do
  @behaviour ExUnit.Formatter

  def suite_started(_opts) do
    :ok
  end

  def suite_finished(:ok, _run_us, _load_us) do
    1
  end

  def case_started(:ok, _test_case) do
    :ok
  end

  def case_finished(:ok, _test_case) do
    :ok
  end

  def test_started(:ok, _test) do
    :ok
  end

  def test_finished(:ok, test) do
		ExUnit.Test[name: name, case: test_case, failure: failure] = test
		{_error, _expectation_error, [{_, _, _, [file: file, line: line]}]} = failure
		# test_name = String.split(name, %r/^test /)

		message = 
			"#{test_case} '#{name}' has damaged your karma.\n\n" <>
			"Please meditate on the following code:\n" <>
			"  #{file}:#{line}, in '#{name}'"
		IO.puts message
		:ok
  end
end
