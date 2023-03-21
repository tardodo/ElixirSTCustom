defmodule ElixirST.Startup do
  use Application

  def start(_type, _args) do
    Mix.Tasks.Compile.Elixir.run(["--force"])
    Supervisor.start_link [], strategy: :one_for_one
  end
end
