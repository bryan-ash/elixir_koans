defmodule Clobber_koans do
  use ExUnit.Case

  test "Removes the koans/ directory" do
    File.mkdir "koans"
    System.cmd "mix clobber_koans"
    refute File.exists? "koans/"
  end
end
