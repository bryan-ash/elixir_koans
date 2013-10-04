defmodule Mix.Tasks.Clobber_koans do
  use Mix.Task

  @shortdoc "Remove the koans directory"

  def run(_) do
    File.rm_rf "koans/"
  end
end

defmodule Mix.Tasks.Gen do
  use Mix.Task

  @shortdoc "Prepare by generating the koans"

  def run(_) do
    unless File.exists? "koans/" do
      IO.puts "Creating a fresh set of koans"
      File.mkdir "koans"
      File.cp_r "src/.", "koans"
    end
  end
end

defmodule Mix.Tasks.Regen do
  use Mix.Task

  @shortdoc "Prepare by generating the koans"

  def run(_) do
    Mix.Task.run "clobber_koans"
    Mix.Task.run "gen"
  end
end

defmodule Mix.Tasks.Walk_the_path do
  use Mix.Task
 
  @shortdoc "Procede along the path to Elixir enlightenment"
 
  def run(_) do
    Mix.Task.run "gen"
    Code.require_file "path_to_enlightenment.exs", "lib"
  end
end
 
defmodule ElixirKoans.Mixfile do
  use Mix.Project
 
  def project do
    [ app: :elixir_koans,
      version: "0.0.1",
      elixir: "~> 0.10.2",
      deps: deps,
      default_task: :walk_the_path ]
  end
 
  # Configuration for the OTP application
  def application do
    []
  end
 
  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    []
  end
end
