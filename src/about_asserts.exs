Code.require_file "test_helper.exs", __DIR__

defmodule AboutAsserts do
  use ExUnit.Case

  # We shall contemplate truth by testing reality, via asserts.
  test "assert truth" do
    assert(false)    # this should be true
  end
end
