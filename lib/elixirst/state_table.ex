defmodule ElixirST.StateTable do
  alias ElixirST.GST
  def createTable(mod) do
    load_paths = Mix.Project.compile_path()
    mod_name = Atom.to_string(mod)

    path = Path.wildcard(load_paths <> "/" <> mod_name <> ".beam")
    # paths =
    #   if length(argv) > 0 do
    #     Enum.map(argv, fn path -> Path.wildcard(load_paths <> "/Elixir*." <> path <> ".beam") end)
    #     |> List.flatten()
    #   else
    #     # or Path.wildcard("projects/*/ebin/**/*.beam")
    #     Path.wildcard(load_paths <> "/Elixir.*.beam")
    #   end

    # if length(path) == 0 do
    #   throw("No paths found for module: #{Enum.join(argv, ", ")}")
    # end

    file =
      case File.read(path) do
        {:ok, file} -> file
        {:error, _} -> throw("Could not read #{path}.")
      end


    # for file <- files do
    #   ElixirST.Retriever.process(file)
    #   ElixirST.Retriever.processGlobal(file)
    # end
    # dbgi_map = %{}
    try do
      # Gets debug_info chunk from BEAM file
      chunks =
        case :beam_lib.chunks(file, [:debug_info]) do
          {:ok, {_mod, chunks}} -> chunks
          {:error, _, error} -> throw({:error, inspect(error)})
        end

      # Gets the (extended) Elixir abstract syntax tree from debug_info chunk
      dbgi_map =
        case chunks[:debug_info] do
          {:debug_info_v1, :elixir_erl, metadata} ->
            case metadata do
              {:elixir_v1, map, _} ->
                # Erlang extended AST available
                map

              {version, _, _} ->
                throw({:error, "Found version #{version} but expected :elixir_v1."})
            end

          x ->
            throw({:error, inspect(x)})
        end


      # Gets the list of session types, which were stored as attributes in the module
      session_types = Keyword.get_values(dbgi_map[:attributes], :global_session_collection)

      session_types_parsed =
        for {_, session_type_string} <- Keyword.values(session_types) do
          GST.string_to_st(session_type_string)
        end

      :ets.new(mod, [:named_table, :public])

      transitions(session_types_parsed, mod)

      :ok

    catch
      {:error, message} ->
        Logger.error("Error while reading BEAM files: " <> message)
    end
  end

  def fetchCurrentState(module) do
    res = :ets.lookup(module, :current_state)

    case res do
      [{:current_state, state}] -> state
    end
  end

  def transitionState(module, state, request, return) do
    req_label = elem(request, 0)
    ret_label = elem(return, 0)

    res = :ets.lookup(module, {state, req_label, ret_label})
    case res do
      [{_, state}] -> :ets.update_element(module, :current_state, {2, state})
    end

    :ok
  end

  defp transitions([parsed_st], module) do
    trans = %{state: nil, func: nil, response: nil, to: nil, root: nil, module: module}

    %GST.Recurse{label: state, body: _} = parsed_st

    new_trans = %{trans | root: state}

    :ets.insert(module, {:current_state, state})

    genTransitions(parsed_st, new_trans)
  end

  defp genTransitions(%GST.Recurse{label: state, body: rest}, trans) do
    # trans = %{state: nil, func: nil, response: nil, to: nil}

    # {state, rest} =
    #   case parsed_st do
    #     %GST.Recurse{label: state, body: rest} -> {state, rest}
    #   end

    new_trans = %{trans | state: state}

    genTransitions(rest, new_trans)

  end

  defp genTransitions(%GST.Branch{branches: branches}, trans) do
    # trans = %{state: nil, func: nil, response: nil, to: nil}

    # {state, branches} =
    #   case parsed_st do
    #     %GST.Recurse{label: state, body: %GST.Branch{branches: branches}} -> {state, branches}
    #   end

    Enum.each(branches, fn ({label, branch}) -> genTransitions(branch, trans) end)
    # new_trans = %{trans | state: state}


  end

  defp genTransitions(%GST.Choice{choices: choices}, trans) do

    Enum.each(choices, fn ({label, choice}) -> genTransitions(choice, trans) end)

  end

  defp genTransitions(%GST.FunCall{label: func, next: rest}, trans) do
    # trans = %{state: nil, func: nil, response: nil, to: nil}

    # {state, rest} =
    #   case parsed_st do
    #     %GST.Recurse{label: state, body: rest} -> {state, rest}
    #   end


    new_trans = %{trans | func: func}

    genTransitions(rest, new_trans)

  end

  defp genTransitions(%GST.Return{label: return, next: rest}, trans) do

    new_trans = %{trans | response: return}

    new_trans =
      case rest do
        %GST.Call_Recurse{label: to} -> %{new_trans | to: to}
        %GST.Terminate{} -> new_trans
        %GST.Recurse{label: to} -> %{new_trans | to: to}
      end

    # create ets entry
    createEntry(new_trans)

    genTransitions(rest, new_trans)

  end

  defp genTransitions(%GST.Call_Recurse{}, trans) do

  end

  defp genTransitions(%GST.Terminate{}, trans) do

  end

  defp createEntry(transition) do
    %{state: state, func: func, response: return, to: to, root: _, module: mod} = transition

    :ets.insert(mod, {{state, func, return}, to})

  end

end
