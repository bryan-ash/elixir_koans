defmodule ElixirKoans do
  def walk_the_path do
    run Enum.reverse(koans)
  end

  defp run( [] ), do: :ok

  defp run( [ koan | rest ] ) do
    Code.require_file koan, "koans"
    run rest
  end

  defp koans, do: [ "about_asserts.exs",
                    "about_lists.exs" ]
end
