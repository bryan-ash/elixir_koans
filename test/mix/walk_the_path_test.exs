defmodule Walk_the_path do
  use ExUnit.Case

  setup do
    System.cmd "mix regen"
    :ok
  end

  test "Shows damaged karma only for the first test" do
    output = System.cmd "mix walk_the_path"
    assert output =~ %r/AboutAsserts 'test assert truth' has damaged your karma./
    refute output =~ %r/test assert with message/
  end
end
