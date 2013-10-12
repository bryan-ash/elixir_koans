Code.require_file "test_helper.exs", __DIR__
Code.require_file "koan_formatter.exs", "lib"

defmodule KoanFormatterTest do
  use ExUnit.Case

  import KoanFormatter

  test "a failure guides the student to the error" do
    output = formatted_test_failure(TestCase, :"test something",
                                    "Believed",
                                    "magical",
                                    "\"unicorns\"",
                                    "seem",
                                    "./koans/about_unicorns.exs", 8)

    assert output =~ %r/TestCase test 'something' has damaged your karma./
    assert output =~ %r/  Believed "unicorns" to seem magical./
    assert output =~ %r/Please meditate on the following code:/m
    assert output =~ %r/\.\/koans\/about_unicorns.exs:8, in test 'something'/
  end

  test "a success is encouraged" do
    output = formatted_test_success(AboutAsserts, :"test assert truth")

    assert output =~ %r/AboutAsserts test 'assert truth' has expanded your awareness./
  end

  test "progress shows 'X' when the first koan is failed" do
    assert progress(KoanFormatter.State.new(successes: 0, failures: 1)) == IO.ANSI.red <> "X" <> IO.ANSI.reset
  end

  test "progress is '.' when the first koan is passed" do
    assert progress(KoanFormatter.State.new(successes: 1, failures: 0)) == IO.ANSI.green <> "." <> IO.ANSI.reset
  end

  test "progress shows '.' for each success, and 'X' for failure" do
    assert ansi_unescape(progress(KoanFormatter.State.new(successes: 3, failures: 1))) == "...X"
  end

  defp ansi_unescape(string), do: Regex.replace(%r/\e\[\d+m/, string, "")
end
