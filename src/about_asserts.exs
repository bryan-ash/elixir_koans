Code.require_file "test_helper.exs", __DIR__

defmodule AboutAsserts do
  use ExUnit.Case

  # We shall contemplate truth by testing reality, via asserts.
  test "assert truth" do
    assert(false)    # this should be true
  end

  # Enlightenment may be more easily achieved with appropriate
  # messages.
  test "assert with message" do
    assert false, "This should be true -- Please fix this"
  end

  # Sometimes we expect a negative outcome
  test "refute a negative assertion" do
    refute true, "This should be false -- Please fix this"
  end
end
