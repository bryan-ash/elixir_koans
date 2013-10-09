Code.require_file "koan_formatter.exs", "lib"
ExUnit.start formatter: KoanFormatter

defmodule ElixirKoans.Helpers do
  defmacro __using__([]) do
    quote do
      use ExUnit.Case

      def __, do: "FILL ME IN"
    end
  end
end
