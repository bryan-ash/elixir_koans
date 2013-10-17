Code.require_file "test_helper.exs", __DIR__

defmodule AboutLists do
  use ElixirKoans.Helpers

  test "matching is used to extract values from a list" do
    [1, x, 3, y] = [1, 2, 3, 4]
    assert x == __
    assert y == __
  end

  test "the first element is the head, the rest is the tail" do
    [ head | tail ] = [1, 2, 3, 4]
    assert head == __
    assert tail == __
  end
end
