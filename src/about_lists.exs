Code.require_file "test_helper.exs", __DIR__

defmodule AboutLists do
  use ExUnit.Case

  test "matching is used to extract values from a list" do
    [1, x, 3, y] = [1, 2, 3, 4]
    assert x == 3 and y == 4
  end
end