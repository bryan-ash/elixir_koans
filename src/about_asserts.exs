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
end
