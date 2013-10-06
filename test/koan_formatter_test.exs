Code.require_file "koan_formatter.exs", "lib"

defmodule KoanFormatterTest do
  use ExUnit.Case

  test "a failure guides the student to the error" do
    output = KoanFormatter.formatted_test_failure(AboutAsserts, :"test assert truth", "./koans/about_asserts.exs", 8)

    assert output =~ %r/Please meditate on the following code:/m
    assert output =~ %r/\.\/koans\/about_asserts.exs:8, in test 'assert truth'/
  end

  test "a success is encouraged" do
    output = KoanFormatter.formatted_test_success(AboutAsserts, :"test assert truth")

    assert output =~ %r/AboutAsserts test 'assert truth' has expanded your awareness./
  end
end