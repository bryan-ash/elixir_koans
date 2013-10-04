defmodule Generating_koans do
  use ExUnit.Case

  defmodule When_koans_directory_exists do
    use ExUnit.Case

    setup do
      File.rm_rf "koans"
      File.mkdir "koans"
    end

    test "'mix gen' does not copy koans" do
      System.cmd "mix gen"
      assert Dir.is_empty? "koans"
    end

    test "'mix regen' copies koans" do
      System.cmd "mix regen"
      refute Dir.is_empty? "koans"
    end
  end

  defmodule When_koans_directory_does_not_exists do
    use ExUnit.Case

    setup do
      File.rm_rf "koans"
    end

    test "'mix gen' copies koans" do
      System.cmd "mix gen"
      refute Dir.is_empty? "koans"
    end

    test "'mix regen' copies koans" do
      System.cmd "mix regen"
      refute Dir.is_empty? "koans"
    end
  end
end

defmodule Dir do
  def is_empty?(path) do
    File.ls(path) == { :ok, [] }
  end
end
