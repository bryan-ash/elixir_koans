defmodule KoanFormatter do
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
    ExUnit.Test[name: description, case: test_case, failure: failure] = test
    {_kind, _reason, [{_, _, _, [file: file, line: line]}]} = failure
    # test_name = String.split(name, %r/^test /)

    IO.puts formatted_test_failure(test_case, description, Path.relative_to_cwd(file), line)
    System.halt(0)
  end

  def formatted_test_failure(test_case, description, file, line) do
		red("#{inspect(test_case)} '#{description}' has damaged your karma.\n\n") <>
			"Please meditate on the following code:\n" <>
			red("  ./#{file}:#{line}, in '#{description}'")
	end

	defp red(string) do
		IO.ANSI.escape("%{red}" <> string)
	end
end
