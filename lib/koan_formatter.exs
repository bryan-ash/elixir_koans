defmodule KoanFormatter do
  @behaviour ExUnit.Formatter

  def suite_started(_opts), do: :ok
  def suite_finished(:ok, _run_us, _load_us), do: 1
  def case_started(:ok, _test_case), do: :ok
  def case_finished(:ok, _test_case), do: :ok
  def test_started(:ok, _test), do: :ok

  def test_finished(:ok,
                    ExUnit.Test[name: test_name,
                                case: test_case,
                                failure: ({:error,
                                           ExUnit.ExpectationError[expected: expected,
                                                                   assertion: assertion,
                                                                   prelude: prelude,
                                                                   expr: expr],
                                           [{_, _, _, [file: file, line: line]}]})]) do
    IO.puts formatted_test_failure(test_case, test_name, prelude, expr, assertion, expected, Path.relative_to_cwd(file), line)
    System.halt(0)
  end

  def test_finished(:ok, ExUnit.Test[name: test_name, case: test_case]) do
    IO.puts formatted_test_success(test_case, test_name)
    :ok
  end

  def formatted_test_failure(test_case, test_name, prelude, expr, assertion, expected, file, line) do
    "#{inspect(test_case)} test '#{description(test_name)}' has damaged your karma.\n" <>
      color("red", "  #{prelude} \"#{expr}\" to #{assertion} #{expected}.\n\n") <>
      "Please meditate on the following code:\n" <>
      color("cyan", "  ./#{file}:#{line}, in test '#{description(test_name)}'")
  end

  def formatted_test_success(test_case, test_name) do
    color("green", "#{inspect(test_case)} test '#{description(test_name)}' has expanded your awareness.")
  end

  defp description(test_name) do
    "test " <> description = to_string(test_name)
    description
  end

  defp color(color, string) do
    IO.ANSI.escape("%{" <> color <> "}" <> string)
  end
end
