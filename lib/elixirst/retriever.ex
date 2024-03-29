defmodule ElixirST.Retriever do
  require Logger
  alias ElixirST.ST
  alias ElixirST.GST

  @moduledoc """
  Retrieves bytecode and (session) typechecks it.
  """

  @doc """
  Input as bytecode from a BEAM file, takes the Elixir AST from the debug_info
  and forwards it to the typechecker.
  """
  @spec process(binary, list) :: list
  def process(bytecode, options \\ []) do
    try do
      # Gets debug_info chunk from BEAM file
      chunks =
        case :beam_lib.chunks(bytecode, [:debug_info]) do
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
      session_types = Keyword.get_values(dbgi_map[:attributes], :session_type_collection)

      session_types_parsed =
        for {{name, arity}, session_type_string} <- Keyword.values(session_types) do
          {{name, arity}, ST.string_to_st(session_type_string)}
        end

      # Retrieve dual session types (as labels)
      duals = Keyword.get_values(dbgi_map[:attributes], :dual_unprocessed_collection)

      dual_session_types_parsed =
        for {{name, arity}, dual_label} <- duals do
          case Keyword.fetch(session_types, dual_label) do
            {:ok, {{_dual_name, _dual_arity}, session_type}} ->
              dual =
                ST.string_to_st(session_type)
                |> ST.dual()

              {{name, arity}, dual}

            :error ->
              throw("Dual session type '#{dual_label}' does not exist")
          end
        end

      function_types = Keyword.get_values(dbgi_map[:attributes], :type_specs)

      # temp_funcs = Keyword.get_values(dbgi_map[:attributes], :temp_type_specs)
      # {_, {args_types, return_type}} = Enum.at(temp_funcs, 0)
      # args_types_converted = ElixirST.TypeOperations.get_type(args_types)
      # return_type_converted = ElixirST.TypeOperations.get_type(return_type)

      all_functions =
        get_all_functions!(dbgi_map)
        |> add_types_to_functions(to_map(function_types))
        # |> IO.inspect

      # Session typechecking of each individual function
      ElixirST.SessionTypechecking.session_typecheck_module(
        all_functions,
        to_map(session_types_parsed ++ dual_session_types_parsed),
        dbgi_map[:module],
        options
      )
    catch
      {:error, message} ->
        Logger.error("Error while reading BEAM files: " <> message)
    end
  end

  @spec processGlobal(binary, list) :: list
  def processGlobal(bytecode, options \\ []) do
    try do
      # Gets debug_info chunk from BEAM file
      chunks =
        case :beam_lib.chunks(bytecode, [:debug_info]) do
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
        for {name, session_type_string} <- Keyword.values(session_types) do
          {name, GST.string_to_st(session_type_string)}
        end


      function_types = Keyword.get_values(dbgi_map[:attributes], :type_specs)

      temp_funcs = Keyword.get_values(dbgi_map[:attributes], :temp_type_specs)
      # {_, {args_types, return_type}} = Enum.at(temp_funcs, 0)
      # args_types_converted = ElixirST.TypeOperations.get_type(args_types)
      # return_type_converted = ElixirST.TypeOperations.get_type(return_type)

      impl_func = Keyword.get_values(dbgi_map[:attributes], :callback_impl)

      name = dbgi_map[:module]
      type = is_atom(name)
      name1 = Atom.to_string(name)
      name2 = String.replace(name1, ".", "_")
      name = String.to_atom(name2)

    #   try do
    #     :ets.new(name, [:named_table, :public])
    #   catch
    #     x ->  IO.puts("THIS IS THE ERROR: #{x}")
    #           :ets.delete(name);
    #           :ets.new(name, [:named_table])
    #   end

    # :ets.insert(name, {"foo", "baba"})
    # app = Application.get_application(dbgi_map[:module])
    # Application.put_env(app, name, "FOO FIGHTERS")
    # env = Application.get_env(app, name)
    # IO.puts(env)

    # :persistent_term.put(name, "foo2")

    # loc = :ets.whereis(name)
    # IO.puts("NAME IS: #{name} and loc is #{loc}")
    # IO.puts("NAME IS: #{name}")

      unless Enum.empty?(impl_func) do
        # {_, callback_func_names} = impl_func
        all_functions =
          get_implemented_callbacks!(dbgi_map, impl_func)
          |> add_many_types_to_functions(function_types)
          # |> IO.inspect



        rest = all_functions

        ElixirST.GlobalSessionTypechecking.session_typecheck_module(
          all_functions,
          session_types_parsed,
          dbgi_map[:module],
          options
        )

        {_, {module, st}} = Enum.at(session_types, 0)
        # s = st
        # :persistent_term.put(name, st)

        all_sts =
          try do
            ret = :persistent_term.get(:global_st)
            ret
          rescue
            x -> :persistent_term.put(:global_st, %{}); %{}

          end

        # all_sts = %{all_sts | name: st}
        all_sts = Map.put(all_sts, name, st)
        :persistent_term.put(:global_st, all_sts)
        # :persistent_term.put(name, "foo!!")


        # try do
        #   :ets.new(:states, [:named_table])
        # catch

        # end

      end

      # all_functions =
      #   get_implemented_callbacks!(dbgi_map, callback_func_names)
      #   |> add_types_to_functions(to_map(function_types))
      #   |> IO.inspect

      # rest = all_functions
      # Session typechecking of each individual function
      # ElixirST.SessionTypechecking.session_typecheck_module(
      #   all_functions,
      #   to_map(session_types_parsed),
      #   dbgi_map[:module],
      #   options
      # )
    catch
      {:error, message} ->
        Logger.error("Error while reading BEAM files: " <> message)
    end
  end

  defp to_map(list) do
    list
    |> Enum.into(%{})
  end

  # Given the debug info chunk from the Beam files,
  # return a list of all functions

  # Structure of [Elixir] functions in Beam
  # {{name, arity}, :def_or_p, meta, [{meta, parameters, guards, body}, case2, ...]}
  # E.g.
  # {{:function1, 1}, :def, [line: 36],
  #  [
  #    {[line: 36], [7777],                       [],       {:__block__, [], [...]}}, # Case 1
  #    {[line: 47], [{:server, [line: 47], nil}], [guards], {...}                  }, # Case 2
  #    ...
  #  ]
  # }
  defp get_all_functions!(dbgi_map) do
    dbgi_map[:definitions]
    |> Enum.map(fn
      {{name, arity}, def_p, meta, function_body} ->
        # Unzipping function_body
        {metas, parameters, guards, bodies} =
          Enum.reduce(function_body, {[], [], [], []}, fn {curr_m, curr_p, curr_g, curr_b}, {accu_m, accu_p, accu_g, accu_b} ->
            {[curr_m | accu_m], [curr_p | accu_p], [curr_g | accu_g], [curr_b | accu_b]}
          end)

        {{name, arity},
         %ST.Function{
           name: name,
           arity: arity,
           def_p: def_p,
           meta: meta,
           cases: length(bodies),
           case_metas: metas,
           parameters: parameters,
           guards: guards,
           bodies: bodies
         }}

      x ->
        throw({:error, "Unknown info for #{inspect(x)}"})
    end)
    |> to_map()
  end

  defp get_implemented_callbacks!(dbgi_map, names) do
    dbgi_map[:definitions]
    |> Enum.filter(fn
      {{name, arity}, _, _, _} -> ((name == :handle_call) and (arity == 4)) or ((name == :handle_cast) and (arity == 3)) end
        # Keyword.has_key?(names, name) and (Keyword.get_values(names, name) != arity)  end
      )
    |> Enum.map(fn
      {{name, arity}, def_p, meta, function_body} ->
        # Unzipping function_body
        {metas, parameters, guards, bodies} =
          Enum.reduce(function_body, {[], [], [], []}, fn {curr_m, curr_p, curr_g, curr_b}, {accu_m, accu_p, accu_g, accu_b} ->
            {[curr_m | accu_m], [curr_p | accu_p], [curr_g | accu_g], [curr_b | accu_b]}
          end)

        {{name, arity},
         %GST.Function{
           name: name,
           arity: arity,
           def_p: def_p,
           meta: meta,
           cases: length(bodies),
           case_metas: metas,
           parameters: parameters,
           guards: guards,
           bodies: bodies
         }}

      x ->
        throw({:error, "Unknown info for #{inspect(x)}"})
    end)
    |> to_map()
  end

  defp add_types_to_functions(all_functions, function_types) do
    for {{name, arity}, function} <- all_functions do
      types = Map.get(function_types, {name, arity}, nil)

      if not is_nil(types) do
        {param_types, return_type} = types
        {{name, arity}, %{function | types_known?: true, return_type: return_type, param_types: param_types}}
      else
        {{name, arity}, function}
      end
    end
    |> to_map()
  end


defp add_many_types_to_functions(all_functions, function_types) do
  for {{name, arity}, function} <- all_functions do
    # types = Map.get(function_types, {name, arity}, nil)
    types = Enum.filter(function_types,
              fn {{n, a}, _} -> n == name and a == arity end)


    # types =
    # accp = []
    # accr = []
    if not Enum.empty?(types) do
      # accp = []
      # accr = []
      # params_t = (fn {_, {param_types, _}} -> param_types end)
      # return_t = (fn {_, {_, return_types}} -> return_types end)
      # params_t = for {_, {param_types, return_type}} <- types do

      #   ^accp = [param_types | accp]
      #   ^accr = [return_type | accr]
      # end
      params_t = Enum.map(types, (fn {_, {param_types, _}} -> param_types end)) |> Enum.reverse()
      return_t = Enum.map(types, (fn {_, {_, return_types}} -> {return_types} end)) |> Enum.reverse()
      {{name, arity}, %{function | types_known?: true, return_type: return_t, param_types: params_t}}
    else
      {{name, arity}, function}
    end
  end
  |> to_map()
end
end
