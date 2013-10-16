defmodule KoanFormatter do
  @timeout 30_000
  @behaviour ExUnit.Formatter

  use GenServer.Behaviour

  defrecord State, successes: 0, failures: 0, remaining: 0, running: true

  ## Behaviour

  def suite_started(opts) do
    { :ok, pid } = :gen_server.start_link(__MODULE__, opts, [])
    pid
  end

  def suite_finished(id, run_us, load_us) do
    :gen_server.call(id, { :suite_finished, run_us, load_us }, @timeout)
  end

  def case_started(id, test_case) do
    :gen_server.cast(id, { :case_started, test_case })
  end

  def case_finished(id, test_case) do
    :gen_server.cast(id, { :case_finished, test_case })
  end

  def test_started(id, test) do
    :gen_server.cast(id, { :test_started, test })
  end

  def test_finished(id, test) do
    :gen_server.cast(id, { :test_finished, test })
  end

  ## Callbacks

  def init(opts) do
    { :ok, State.new(opts) }
  end

  def handle_call({ :suite_finished, _run_us, _load_us }, _from, state = State[]) do
    IO.puts "\n" <> progress_along_the_path(state)
    { :stop, :normal, state.failures, state }
  end

  def handle_call(reqest, from, state) do
    super(reqest, from, state)
  end

  def handle_cast({ :test_finished, ExUnit.Test[failure: nil] = test }, state = State[running: true]) do
    IO.puts formatted_test_success(test.case, test.name)
    { :noreply, state.update_successes(&(&1 + 1)) }
  end

  def handle_cast({ :test_finished, test }, state = State[running: true]) do
    ExUnit.Test[failure: {:error, expectation, location}] = test
    [{_, _, _, [file: file, line: line]}] = location

    IO.puts formatted_test_failure(test, expectation, file, line)
    { :noreply, state.update_failures(&(&1 + 1)).update_running(fn(_) -> false end) }
  end

  def handle_cast({ :test_finished, _test }, state = State[running: false]) do
    { :noreply, state.update_remaining(&(&1 + 1)) }
  end

  def handle_cast(request, state) do
    super(request, state)
  end

  def formatted_test_failure(test, expectation, file, line) do
    statement_of_damaged_karma(test) <>
      formatted_expectation(expectation) <>
      guided_direction(file, line, test)
  end

  def statement_of_damaged_karma(test) do
    "#{inspect(test.case)} test '#{description(test.name)}' has damaged your karma.\n"
  end

  def formatted_expectation(ExUnit.ExpectationError[prelude: prelude,
                                                    actual: actual,
                                                    assertion: assertion,
                                                    expected: expected]) do
    color("red", "  #{prelude} #{actual} to #{assertion} #{expected}.\n\n")
  end

  def guided_direction(file, line, test) do
    "Please meditate on the following code:\n" <>
      color("cyan", "  #{Path.relative_to_cwd(file)}:#{line}, in test '#{description(test.name)}'")
  end

  def formatted_test_success(test_case, test_name) do
    color("green", "#{inspect(test_case)} test '#{description(test_name)}' has expanded your awareness.")
  end

  def progress_along_the_path(state = State[]) do
    color("green", "your progess so far [") <>
      progress(state) <>
      color("green", "]")
  end

  def progress(State[successes: 0, failures: 0, remaining: 0]), do: ""

  def progress(State[successes: 0, failures: failures] = state) when failures > 0 do
    color("red", "X") <> path_remaining(state.remaining)
  end

  def progress(State[successes: successes] = state) when successes > 0 do
    color("green", ".") <> progress(state.update_successes(&(&1 - 1)))
  end

  def path_remaining(0), do: ""
  def path_remaining(remaining), do: color("cyan", "_") <> path_remaining(remaining - 1)

  defp description(test_name) do
    "test " <> description = to_string(test_name)
    description
  end

  defp color(color, string) do
    IO.ANSI.escape("%{" <> color <> "}" <> string)
  end
end
