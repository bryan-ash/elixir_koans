defmodule KoanFormatter do
  @behaviour ExUnit.Formatter

  use GenServer.Behaviour

  defrecord State, successes: 0, failures: 0

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
    { :stop, :normal, state.failures, state }
  end

  def handle_call(reqest, from, state) do
    super(reqest, from, state)
  end

  def handle_cast({ :test_started, ExUnit.Test[] = _test }, state) do
    { :noreply, state }
  end

  def handle_cast({ :test_finished,
                    ExUnit.Test[failure: ({:error,
                                           ExUnit.ExpectationError[] = record,
                                                  [{test_case, test_name, _, [file: file, line: line]}]})] },
                  _state = State[]) do
    IO.puts formatted_test_failure(test_case, test_name,
                                   record.prelude, record.expected, record.actual, record.assertion,
                                   Path.relative_to_cwd(file), line)
    System.halt(0)
  end

  def handle_cast({ :test_finished, ExUnit.Test[name: test_name, case: test_case] }, _state = State[]) do
    IO.puts formatted_test_success(test_case, test_name)
    :ok
  end

  def handle_cast({ :case_started, ExUnit.TestCase[name: _name] }, state) do
    { :noreply, state }
  end

  def handle_cast({ :case_finished, _test_case }, state) do
    { :noreply, state }
  end

  def handle_cast(request, state) do
    super(request, state)
  end

  def formatted_test_failure(test_case, test_name, prelude, expected, actual, assertion, file, line) do
    "#{inspect(test_case)} test '#{description(test_name)}' has damaged your karma.\n" <>
      color("red", "  #{prelude} #{actual} to #{assertion} #{expected}.\n\n") <>
      "Please meditate on the following code:\n" <>
      color("cyan", "  ./#{file}:#{line}, in test '#{description(test_name)}'")
  end

  def formatted_test_success(test_case, test_name) do
    color("green", "#{inspect(test_case)} test '#{description(test_name)}' has expanded your awareness.")
  end

  defp description(test_name) do
    "test " <> description = to_string(test_name)
    description
  end

  defp color(color, string) do
    IO.ANSI.escape("%{" <> color <> "}" <> string)
  end
end
