defmodule Walk_the_path do
  use ExUnit.Case

  setup do
    System.cmd "mix regen"
    :ok
  end

  test "Shows damaged karma for 'test the truth'" do
    output = System.cmd "mix walk_the_path"
    assert output =~ %r/AboutAsserts 'test assert truth' has damaged your karma./
  end
end
