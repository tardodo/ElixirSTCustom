defmodule Mix.Tasks.Run.Session do
  use Mix.Task

  @moduledoc """
  Use `mix sessions` to run ElixirST for all module, or `mix sessions [module name]` to run ElixirST only for a specific module.
  """

  # @spec run([binary]) :: list
  def run(args) do
    {_opts, argv} = OptionParser.parse!(args, switches: [expression_typing: :boolean])
    # options
    # expression_typing = Keyword.get(opts, :expression_typing, true)

    # Mix.Tasks.Iex.run(["-S", "mix"])
    # Mix.Tasks.Iex.run(["-S mix"])
    # load_paths = Mix.Project.compile_path()

    # paths =
    #   if length(argv) > 0 do
    #     Enum.map(argv, fn path -> Path.wildcard(load_paths <> "/Elixir*." <> path <> ".beam") end)
    #     |> List.flatten()
    #   else
    #     # or Path.wildcard("projects/*/ebin/**/*.beam")
    #     Path.wildcard(load_paths <> "/Elixir.*.beam")
    #   end

    # if length(paths) == 0 do
    #   throw("No paths found for module: #{Enum.join(argv, ", ")}")
    # end

    # files =
    #   for path <- paths do
    #     case File.read(path) do
    #       {:ok, file} -> file
    #       {:error, _} -> throw("Could not read #{path}.")
    #     end
    #   end

    # for file <- files do
    #   ElixirST.Retriever.process(file)
    #   ElixirST.Retriever.processGlobal(file)
    # end

    # try do
    #   :ets.new(:states, [:named_table])
    # catch
    #   x ->  IO.puts("THIS IS THE ERROR: #{x}")
    #         :ets.delete(:states);
    #         :ets.new(:states, [:named_table, read_concurrency: true])
    # end

    # :ets.insert(:states, {"foo", "baba"})
  end
end
