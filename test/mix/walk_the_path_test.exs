defmodule Walk_the_path do
  use ExUnit.Case

  test "Shows damaged karma only for the first test" do
    System.cmd "mix regen"
    output = System.cmd "mix walk_the_path"
    assert output =~ %r/AboutAsserts test 'assert truth' has damaged your karma./
    refute output =~ %r/test assert with message/
    assert output =~ %r/\[X_*\]/
  end
end
