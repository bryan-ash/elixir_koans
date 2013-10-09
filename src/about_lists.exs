Code.require_file "test_helper.exs", __DIR__

defmodule AboutLists do
  use ElixirKoans.Helpers

  test "matching is used to extract values from a list" do
    [1, x, 3, 4] = [1, 2, 3, 4]
    assert x == __
  end
end
