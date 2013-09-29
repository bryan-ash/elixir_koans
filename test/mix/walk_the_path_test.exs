defmodule Walk_the_path do
  use ExUnit.Case

  setup do
    System.cmd "mix regen"
    :ok
  end

  test "Shows damaged karma for 'test the truth'" do
    output = System.cmd "mix walk_the_path"
    assert output =~ %r/AboutAsserts 'test the truth' has damaged your karma./
    assert output =~ %r/Please meditate on the following code:\n/
    assert output =~ %r/koans\/about_asserts.exs:7, in 'test the truth'/
  end
end
