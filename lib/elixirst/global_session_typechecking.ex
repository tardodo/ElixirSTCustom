defmodule ElixirST.GlobalSessionTypechecking do
  alias ElixirST.GST
  alias ElixirST.TypeOperations
  require GST
  require ElixirST.TypeOperations
  require Logger

  @moduledoc """
  Elixir code is typechecked against a pre-define session type.
  """

  @spec session_typecheck_module(
    %{GST.name_arity() => GST.Function.t()},
     GST.global_session_type(),
    atom(),
    list
  ) :: list
def session_typecheck_module(
  all_functions,
  module_session_type,
  _module_name,
  _options \\ []
) do
# Logger.debug("Starting session typechecking for module #{inspect(module_name)}")

  [{mod, expected_session_type}] = module_session_type
  all_functions = Map.delete(all_functions, {:init, 1})

# get functions
  functions = Map.values(all_functions)

  env = %{
  # :ok or :error or :warning
  :state => :ok,
  # error message
  :error_data => nil,
  # :x => :atom
  :variable_ctx => %{},
  # Expected session type
  # rec X.(!A().X)
  :session_type => expected_session_type,
  # Expected type
  :type => :any,
  # {name, arity} => %GST.Function
  :functions => all_functions,

  :rec_label => []
  # {name, arity} => rec X.(!A().X)
  # :function_session_type_ctx => function_session_type
  }

  val = check_session_type(expected_session_type, all_functions, env)

  Logger.info("Global Session typechecking for #{mod} terminated successfully")
end

  # Session type checking a whole module, which may include multiple functions with multiple session type definitions
  @spec session_typecheck_module1(
          %{GST.name_arity() => GST.Function.t()},
           GST.global_session_type(),
          atom(),
          list
        ) :: list
  def session_typecheck_module1(
        all_functions,
        module_session_type,
        _module_name,
        _options \\ []
      ) do
    # Logger.debug("Starting session typechecking for module #{inspect(module_name)}")

    [{mod, expected_session_type}] = module_session_type
    all_functions = Map.delete(all_functions, {:init, 1})

    # get functions
      functions = Map.values(all_functions)

      env = %{
        # :ok or :error or :warning
        :state => :ok,
        # error message
        :error_data => nil,
        # :x => :atom
        :variable_ctx => %{},
        # Expected session type
        # rec X.(!A().X)
        :session_type => expected_session_type,
        # Expected type
        :type => :any,
        # {name, arity} => %GST.Function
        :functions => all_functions,

        :rec_label => []
        # {name, arity} => rec X.(!A().X)
        # :function_session_type_ctx => function_session_type
      }

      val = check_session_type(expected_session_type, all_functions, env)
      # get outer recursive label
      outer_recursive =
        case expected_session_type do
          %GST.Recurse{label: label, body: _ , outer_recurse: _} -> label
          _ -> nil
        end

      # get branches
      has_branches =
        case expected_session_type do
          %GST.Recurse{label: _, body: %GST.Branch{branches: branches}} -> branches
          _ -> throw("No Branch Specified")
        end

      # get number of functions (including repeated function use through pattern matching)
      num_funcs =
        Map.values(all_functions)
        |> Enum.reduce(0, fn x, acc -> x.cases + acc end)

      # same number of branches as functions
      if map_size(has_branches) == num_funcs do

        # go through each function
        for function <- functions do

          %GST.Function{
            types_known?: types_known?
          } = function

          if not types_known? do
            throw("Function #{function.name}/#{function.arity} has unknown return type. Use @spec to set parameter and return types.")
          end

          # go through each case of function
          resulting_envs =
            for {body, param, param_t} <- Enum.zip([function.bodies, function.parameters, function.param_types]) do

              # get request label and parameters
              req_param = Enum.at(param, 0)
              req_param_t = Enum.at(param_t, 0)
              vars = get_vars(req_param, req_param_t)
              keys = Keyword.keys(vars)
              label = Enum.at(keys, 0)

              # check for a match
              if has_branches[label] do
                # match with case
                case has_branches[label] do
                  %GST.FunCall{label: label, next: remaining_st, types: t} ->

                    # check parameter number equality
                    if length(t) == (length(keys)-1) do

                      # if has parameters, type check
                      rest_st =
                        if( length(t) == 0) do
                          remaining_st
                        else
                          [_  | stTypes] = vars
                          stTypes = Keyword.values(stTypes)
                          res = List.zip([stTypes, t])
                          # |> Enum.reduce(fn x -> x end)
                          nr = Enum.map(res, fn {x, y} -> unless TypeOperations.equal?(x, y) do throw("Parameter types are not equal in Session Type") end end)

                          remaining_st
                        end

                      # build the variable context for function
                      variable_ctx =
                        Enum.zip(param, param_t)
                        |> Enum.map(fn {var, type} -> TypeOperations.get_vars(var, type) end)
                        |> List.flatten()
                        |> Enum.into(%{})

                      # set branch environment
                      branch_env = %{env | variable_ctx: variable_ctx, session_type: rest_st}

                      # traverse function and typecheck
                      {_ast, res_env} = Macro.prewalk(body, branch_env, &typecheck/2)

                      # ends with recursive call, see if outer recursive
                      res_env =
                        case res_env.session_type do
                          %GST.Call_Recurse{label: x} when x == outer_recursive ->  %{res_env | session_type: %GST.Terminate{}}
                          %GST.Terminate{} = y -> y
                        end

                      res_env
                    else
                      throw("Number of Parameters not equal")

                    end
                  x -> x
                end
              end
            end

          # check session types have been fulfilled
          for func <- resulting_envs do
            if(func.session_type != %GST.Terminate{}) do
              throw("Unresolved session type")
            end
          end
        end

      else
        Logger.error("Session typechecking for #{mod} found an error. ")
        Logger.error("Number of callbacks in branch does not match")
        throw("Number of callbacks in branch does not match")
      end

      Logger.info("Global Session typechecking for #{mod} terminated successfully")
  end


  def check_session_type(session_type, functions, env) do
    rec_label =
      case session_type do
        %GST.Recurse{label: label, body: _ , outer_recurse: _} -> label
        _ -> nil
      end

    recurse_labels = env.rec_label
    env = %{env | rec_label: [rec_label | recurse_labels] }
    # get branches
    has_branches =
      case session_type do
        %GST.Recurse{label: _, body: %GST.Branch{branches: branches}} -> branches
        _ -> throw("No Branch Specified")
      end

    # get number of functions with corresponding state (including repeated function use through pattern matching)
    num_funcs =
      Map.values(functions)
      |> Enum.reduce(0, fn x, acc ->
        num = Enum.reduce(x.parameters, 0, fn p, a ->
          state = Enum.at(p, 0)
          state =
            case state do
              {:_, _, nil} -> :_
              _ -> state
            end
          if state == rec_label or state == :_ do
            a + 1
          else
            a
          end
        end)
        num + acc
      end)

    # function = lookup_function!(functions, {:handle_call, 4})
    # same number of branches as functions
    if map_size(has_branches) == num_funcs do

      # go through each function
      resulting_st =
      for function <- Map.values(functions) do

        %GST.Function{
          types_known?: types_known?
        } = function

        if not types_known? do
          throw("Function #{function.name}/#{function.arity} has unknown return type. Use @spec to set parameter and return types.")
        end

        # go through each case of function
        resulting_envs =
          for {body, param, param_t} <- Enum.zip([function.bodies, function.parameters, function.param_types]) do

            # get request label and parameters
            req_param = Enum.at(param, 1)
            req_param_t = Enum.at(param_t, 1)
            vars = get_vars(req_param, req_param_t)
            keys = Keyword.keys(vars)
            label = Enum.at(keys, 0)

            # get state
            state = Enum.at(param, 0)
            state =
              case state do
                {:_, _, nil} -> :_
                _ -> state
              end
            is_correct_state = if (state == rec_label) do true else false end
            has_label = if has_branches[label] do true else false end

            # check for a match
            if state == rec_label or state == :_ do
              if has_label do
                # match with case
                case has_branches[label] do
                  %GST.FunCall{label: label, next: remaining_st, types: t} ->

                    # check parameter number equality
                    if length(t) == (length(keys)-1) do

                      # if has parameters, type check
                      rest_st =
                        if( length(t) == 0) do
                          remaining_st
                        else
                          [_  | stTypes] = vars
                          stTypes = Keyword.values(stTypes)
                          res = List.zip([stTypes, t])
                          # |> Enum.reduce(fn x -> x end)
                          nr = Enum.map(res, fn {x, y} -> unless TypeOperations.equal?(x, y) do throw("Parameter types are not equal in Session Type") end end)

                          remaining_st
                        end

                      # build the variable context for function
                      variable_ctx =
                        Enum.zip(param, param_t)
                        |> Enum.map(fn {var, type} -> TypeOperations.get_vars(var, type) end)
                        |> List.flatten()
                        |> Enum.into(%{})

                      # set branch environment
                      branch_env = %{env | variable_ctx: variable_ctx, session_type: rest_st}

                      # traverse function and typecheck
                      {_ast, res_env} = Macro.prewalk(body, branch_env, &typecheck/2)

                      # case res_env.session_type do
                      #   %GST.Call_Recurse{label: x} when x == rec_label ->  %{res_env | session_type: %GST.Terminate{}}
                      #   # %GST.Terminate{} = y -> y
                      #   %GST.Recurse{} = _ ->  check_session_type(res_env.session_type, functions, res_env)
                      # end


                      # ends with recursive call, see if outer recursive
                      res_env =
                        case res_env.session_type do
                          %GST.Call_Recurse{label: x} ->  %{res_env | session_type: %GST.Terminate{}}
                          %GST.Terminate{} = y -> res_env
                          _ -> res_env
                        end

                      res_env
                    else
                      throw("Number of Parameters not equal")

                    end
                  x -> x
                end
              else
                # %{session_type: %GST.Terminate{}}
                throw("Function does not match Session Type")
              end
            else
              %{session_type: %GST.Terminate{}}
            end
          end

        # check session types have been fulfilled
        # for func <- resulting_envs do
        #   if(func.session_type != %GST.Terminate{}) do
        #     throw("Unresolved session type")
        #   end
        # end

        final_st =
          Enum.reduce_while(resulting_envs, hd(resulting_envs).session_type,
          fn st_env, acc ->
            if st_env.session_type == %GST.Terminate{} do
              {:cont, st_env.session_type}
            else
              res = check_session_type(st_env.session_type, functions, st_env)
              {:cont, res}
            end

          end)
          # for func <- resulting_envs do
          #   resulting_st =
          #     case func.session_type do

          #     end
          # end

        final_st
      # end
      end
      result =
        Enum.reduce_while(resulting_st, hd(resulting_st),
        fn st, acc ->
          if st == %GST.Terminate{} do
            {:cont, st}
          else
            throw("Could not evaluate session type")
          end
        end)
      result
    else
      Logger.error("Session typechecking for  found an error. ")
      Logger.error("Number of callbacks in branch does not match")
      throw("Number of callbacks in branch does not match")
    end
  end



  @types [
    :any,
    :atom,
    :binary,
    :boolean,
    :number,
    :pid,
    :string,
    :no_return,
    nil
  ]

  def get_vars(var, type)
  def get_vars(_, :any), do: []
  def get_vars({op, _, _}, type) when op not in [:{}, :%{}, :=, :_, :|], do: {op, type}
  def get_vars({:_, _, _}, _type), do: []
  def get_vars({:=, _, [_arg1, _arg2]}, _), do: {:error, "'=' is not supported"}
  def get_vars([], {:list, _type}), do: []
  def get_vars([{:|, _, [operand1, operand2]}], {:list, type}), do: [get_vars(operand1, type), get_vars(operand2, {:list, type})]
  def get_vars({:|, _, [operand1, operand2]}, {:list, type}), do: [get_vars(operand1, type), get_vars(operand2, {:list, type})]
  def get_vars(op, {:list, type}) when is_list(op), do: Enum.map(op, fn x -> get_vars(x, type) end)
  def get_vars(_, {:list, _}), do: {:error, "Incorrect type specification"}
  def get_vars([], _), do: {:error, "Incorrect type specification"}
  def get_vars({:|, _, _}, _), do: {:error, "Incorrect type specification"}

  def get_vars({:%{}, _, op}, {:map, {_, value_types}}),
    do:
      Enum.zip(op, value_types)
      |> Enum.map(fn {{_, value}, value_type} -> get_vars(value, value_type) end)

  def get_vars({:%{}, _, _}, _), do: {:error, "Incorrect type specification"}
  def get_vars(_, {:map, {_, _}}), do: {:error, "Incorrect type specification"}
  def get_vars({:{}, _, ops}, {:tuple, type_list}), do: get_vars_tuple(ops, type_list)
  def get_vars(ops, {:tuple, type_list}) when is_tuple(ops), do: get_vars_tuple(Tuple.to_list(ops), type_list)
  def get_vars({:{}, _, _}, _), do: {:error, "Incorrect type specification"}
  def get_vars(_, {:tuple, _}), do: {:error, "Incorrect type specification"}

  def get_vars(value, type) when type in @types or is_atom(type) do

    literal =
      (is_nil(value) and type == nil) or
        (is_boolean(value) and type == :boolean) or
        (is_number(value) and type == :number) or
        (is_pid(value) and type == :pid) or
        (is_binary(value) and type == :binary) or
        (is_atom(value) and type == :atom)

    if literal do
      {value, type}
    else
      {:error, "Incorrect type specification"}
    end
  end

  def get_vars(_, _), do: {:error, "Incorrect type specification"}

  def get_vars_tuple(ops, type_list) do
    if length(ops) === length(type_list),
      do: Enum.zip(ops, type_list) |> Enum.map(fn {var, type} -> get_vars(var, type) end),
      else: {:error, "The number of parameters in tuple does not match the number of types"}
  end

  @spec session_typecheck_by_function(GST.Function.t(), map()) :: no_return()
  def session_typecheck_by_function(%GST.Function{} = function, env) do
    %GST.Function{
      name: name,
      arity: arity,
      bodies: bodies,
      return_type: expected_return_type,
      parameters: parameters,
      param_types: param_types
    } = function
  #   for {ast, params} <- List.zip([bodies, parameters]) do
  #     t = ast
  #     p = params
  #   end
  # end
    all_results =
      for {ast, params, params_t} <- List.zip([bodies, parameters, param_types]) do
        # Initialize the variable context with the parameters and their types

        variable_ctx =
          Enum.zip(params, params_t)
          |> Enum.map(fn {var, type} -> TypeOperations.get_vars(var, type) end)
          # todo: currently ignoring malformed spec types: {:error, "Incorrect type specification"}
          |> List.flatten()
          |> Enum.into(%{})

        env = %{env | variable_ctx: variable_ctx}

        {_ast, res_env} = Macro.prewalk(ast, env, &typecheck/2)

        res_env
      end

    Enum.reduce_while(all_results, 0, fn result, acc ->
    # Enum.reduce_while(all_results, hd(all_results), fn result, acc ->
      case result[:state] do
        :error ->
          {:halt, result}

        _ ->
          # Check return type
          {return_t} = Enum.at(expected_return_type, acc)
          same_type = TypeOperations.equal?(result[:type], return_t)

          cond do
            result[:session_type] != %GST.Terminate{} ->
              {:halt,
               %{
                 result
                 | state: :error,
                   error_data: "Unfulfilled session type for #{name}/#{arity} is " <> ST.st_to_string_current(result[:session_type])
               }}

            same_type == false ->
              {:halt,
               %{
                 result
                 | state: :error,
                   error_data:
                     "Return type for #{name}/#{arity} is #{TypeOperations.string(result[:type])} but expected " <>
                       TypeOperations.string(expected_return_type)
               }}

            true ->
              {:cont, result}
          end
      end
    end)
  end

  @spec typecheck(GST.ast(), map()) :: {GST.ast(), map()}
  def typecheck(
        _node,
        %{
          state: :error,
          error_data: _error_data,
          variable_ctx: _,
          session_type: _,
          type: _,
          functions: _,
          function_session_type_ctx: _
        } = env
      ) do
    # Logger.error("ElixirST Error! " <> error_data)
    {nil, env}
  end

  # Block
  def typecheck({:__block__, meta, args}, env) do
    # Logger.debug("Typechecking: Block")
    node = {:__block__, meta, nil}

    env =
      Enum.reduce_while(args, env, fn current_node, env_acc ->
        {_ast, new_env} = Macro.prewalk(current_node, env_acc, &typecheck/2)

        case new_env[:state] do
          :error ->
            {:halt, new_env}

          _ ->
            {:cont, %{new_env | variable_ctx: Map.merge(env_acc[:variable_ctx], new_env[:variable_ctx])}}
        end
      end)

    {node, env}
  end

  # Literals
  def typecheck(node, env)
      when is_atom(node) or is_number(node) or is_binary(node) or is_boolean(node) or
             is_float(node) or is_integer(node) or is_nil(node) or is_pid(node) do
    # Logger.debug("Typechecking: Literal: #{inspect(node)} #{TypeOperations.typeof(node)}")

    {node, %{env | type: TypeOperations.typeof(node)}}
  end

  # return
  def typecheck({:{}, meta, [:reply | args]}, env) do
    node = {:{}, meta, []}

    {types_list, new_env} =
      Enum.map(args, fn arg -> elem(Macro.prewalk(arg, env, &typecheck/2), 1) end)
      |> Enum.reduce_while({[], env}, fn result, {types_list, env_acc} ->
        case result[:state] do
          :error ->
            {:halt, {[], result}}

          _ ->
            {:cont, {types_list ++ [result[:type]], %{env_acc | variable_ctx: Map.merge(env_acc[:variable_ctx], result[:variable_ctx] || %{})}}}
        end
      end)

    replyType = hd(types_list)
    replyType = [replyType]
    %GST.Return{label: :reply, types: expected_types, next: remaining_st} =
      case env.session_type do
        %GST.Return{label: :reply} = st ->
          st

        %GST.Choice{choices: choices} ->
          if choices[:reply] do
            choices[:reply]
          else
            throw(
              # {:error,
               "Cannot match return in choice"
            )
          end

        _ ->
          throw("Error in return or choice.")
      end

    if TypeOperations.equal?(expected_types, replyType) do
      remaining_st
    else
      throw("Types do not match for return")
    end

    # rest_st =
    #   case env.session_type do
    #     %GST.Return{label: :reply, next: remaining_st, types: st_types} ->
    #       if TypeOperations.equal?(st_types, replyType) do
    #         remaining_st
    #       else
    #         throw("Types do not match for return")
    #       end
    #   end
    types_list = [:atom | types_list]
    {node, %{new_env | type: {:tuple, types_list}, session_type: remaining_st}}
  end

  # Tuples (of size 2)
  def typecheck({arg1, arg2}, env) do
    node = {:{}, [], [arg1, arg2]}
    typecheck(node, env)
  end

  # return
  def typecheck({:{}, meta, [:noreply | args]}, env) do
    node = {:{}, meta, []}

    {types_list, new_env} =
      Enum.map(args, fn arg -> elem(Macro.prewalk(arg, env, &typecheck/2), 1) end)
      |> Enum.reduce_while({[], env}, fn result, {types_list, env_acc} ->
        case result[:state] do
          :error ->
            {:halt, {[], result}}

          _ ->
            {:cont, {types_list ++ [result[:type]], %{env_acc | variable_ctx: Map.merge(env_acc[:variable_ctx], result[:variable_ctx] || %{})}}}
        end
      end)

    %GST.Return{label: :noreply, types: [], next: remaining_st} =
      case env.session_type do
        %GST.Return{label: :noreply, types: []} = st ->
          st

        %GST.Choice{choices: choices} ->
          if choices[:noreply] do
            choices[:noreply]
          else
            throw(
              # {:error,
               "Cannot match return in choice"
            )
          end

        _ ->
          throw( "Error in return for NOREPLY.")
      end
    # rest_st =
    #   case env.session_type do
    #     %GST.Return{label: :noreply, next: remaining_st, types: []} -> remaining_st

    #     _ -> throw("Noreply return cannot have any return types")
    #       # if TypeOperations.equal?(st_types, types_list) do
    #         # remaining_st
    #       # else
    #       #   throw("Types do not match for return")
    #       # end
    #   end
    types_list = [:atom | types_list]
    {node, %{new_env | type: {:tuple, types_list}, session_type: remaining_st}}
  end

  # return
  def typecheck({:{}, meta, [:stop | args]}, env) do
    node = {:{}, meta, []}

    {types_list, new_env} =
      Enum.map(args, fn arg -> elem(Macro.prewalk(arg, env, &typecheck/2), 1) end)
      |> Enum.reduce_while({[], env}, fn result, {types_list, env_acc} ->
        case result[:state] do
          :error ->
            {:halt, {[], result}}

          _ ->
            {:cont, {types_list ++ [result[:type]], %{env_acc | variable_ctx: Map.merge(env_acc[:variable_ctx], result[:variable_ctx] || %{})}}}
        end
      end)

    replyTypes =
      if length(types_list) == 3 do
        [first, sec | _] = types_list
        [first, sec]
      else
        [first | _] = types_list
        [first]
      end

    %GST.Return{label: :stop, types: expected_types, next: remaining_st} =
      case env.session_type do
        %GST.Return{label: :stop} = st ->
          st

        %GST.Choice{choices: choices} ->
          if choices[:stop] do
            choices[:stop]
          else
            throw(
              # {:error,
                "Cannot match return in choice"
            )
          end

        _ ->
          throw( "Error in return or choice.")
      end

    if TypeOperations.equal?(expected_types, replyTypes) do
      remaining_st
    else
      throw("Types do not match for return")
    end
    # rest_st =
    #   case env.session_type do
    #     %GST.Return{label: :stop, next: remaining_st, types: st_types} ->
    #       if TypeOperations.equal?(st_types, replyTypes) do
    #         remaining_st
    #       else
    #         throw("Types do not match for return")
    #       end
    #   end

    types_list = [:atom | types_list]
    {node, %{new_env | type: {:tuple, types_list}, session_type: remaining_st}}
  end

  # Tuples
  def typecheck({:{}, meta, args}, env) when is_list(args) do
    node = {:{}, meta, []}

    {types_list, new_env} =
      Enum.map(args, fn arg -> elem(Macro.prewalk(arg, env, &typecheck/2), 1) end)
      |> Enum.reduce_while({[], env}, fn result, {types_list, env_acc} ->
        case result[:state] do
          :error ->
            {:halt, {[], result}}

          _ ->
            {:cont, {types_list ++ [result[:type]], %{env_acc | variable_ctx: Map.merge(env_acc[:variable_ctx], result[:variable_ctx] || %{})}}}
        end
      end)

    {node, %{new_env | type: {:tuple, types_list}}}
  end


  # Lists and List operations
  def typecheck([], env) do
    {[], %{env | type: {:list, :any}}}
  end

  def typecheck([{:|, meta, [operand1, operand2]}], env) do
    node = {:|, meta, []}
    process_binary_operations(node, meta, :|, operand1, operand2, {:list, nil}, false, false, env)
  end

  def typecheck(node, env) when is_list(node) do
    typechecked_nodes = Enum.map(node, fn t -> elem(Macro.prewalk(t, env, &typecheck/2), 1) end)

    result =
      Enum.reduce_while(typechecked_nodes, %{hd(typechecked_nodes) | type: hd(typechecked_nodes)[:type]}, fn result, env_acc ->
        case result[:state] do
          :error ->
            {:halt, result}

          _ ->
            if TypeOperations.equal?(result[:type], env_acc[:type]) do
              {:cont, %{result | variable_ctx: Map.merge(env_acc[:variable_ctx], result[:variable_ctx] || %{})}}
            else
              {:halt,
               %{
                 result
                 | state: :error,
                   error_data: "Malformed list (" <> inspect(result[:type]) <> ", " <> inspect(env_acc[:type]) <> "): " <> inspect(node)
               }}
            end
        end
      end)

    {[], %{result | type: {:list, result[:type]}}}
  end

  def typecheck({{:., meta1, [:erlang, operator]}, meta2, [arg1, arg2]}, env)
      when operator in [:++, :--] do
    # Logger.debug("Typechecking: Erlang #{operator}")
    node = {{:., meta1, []}, meta2, []}

    {node, result_env} = process_binary_operations(node, meta2, operator, arg1, arg2, {:list, nil}, true, false, env)

    if result_env[:state] == :error do
      {node, result_env}
    else
      case result_env[:type] do
        {:list, _} ->
          {node, result_env}

        _ ->
          {node,
           %{
             result_env
             | state: :error,
               error_data: error_message("Expected type of list but found " <> TypeOperations.string(result_env[:type]), meta2)
           }}
      end
    end
  end

  # Arithmetic operations
  def typecheck({{:., meta1, [:erlang, operator]}, meta2, [arg1, arg2]}, env)
      when operator in [:+, :-, :*, :/] do
    # Logger.debug("Typechecking: Erlang #{operator}")
    node = {{:., meta1, []}, meta2, []}

    process_binary_operations(node, meta2, operator, arg1, arg2, :number, false, false, env)
  end

  # too complex in extended elixir: [:and, :or]

  # Elixir format:          [:==, :!=,   :===,   :!== ,  :>, :<, :<=,   :>=  ]
  # Extended Elixir format: [:==, :"/=", :"=:=", :"=/=", :>, :<, :"=<", :">="]
  def typecheck({{:., meta1, [:erlang, operator]}, meta2, [arg1, arg2]}, env)
      when operator in [:==, :"/=", :"=:=", :"=/=", :>, :<, :"=<", :>=] do
    node = {{:., meta1, []}, meta2, []}
    # improve: convert operator from extended elixir to elixir
    process_binary_operations(node, meta2, operator, arg1, arg2, :any, false, true, env)
  end

  # Not
  def typecheck({{:., _meta1, [:erlang, :not]}, meta2, [arg]} = node, env) do
    process_unary_operations(node, meta2, arg, :boolean, env)
  end

  # Negate
  def typecheck({{:., _meta1, [:erlang, :-]}, meta2, [arg]}, env) do
    # Logger.debug("Typechecking: Erlang negation")

    node = {nil, meta2, []}
    process_unary_operations(node, meta2, arg, :number, env)
  end

  def typecheck({{:., _meta1, [:erlang, erlang_function]}, meta2, _arg}, env)
      when erlang_function not in [:send, :self] do
    # Logger.debug("Typechecking: Erlang others #{erlang_function} (not supported)")
    node = {nil, meta2, []}

    {node, %{env | state: :error, error_data: error_message("Unknown erlang function #{inspect(erlang_function)}", meta2)}}
  end

  # Binding operator
  def typecheck({:=, meta, [pattern, expr]}, env) do
    # Logger.debug("Typechecking: Binding operator (i.e. =)")
    node = {:=, meta, []}

    {_expr_ast, expr_env} = Macro.prewalk(expr, env, &typecheck/2)

    case expr_env[:state] do
      :error ->
        {node, expr_env}

      _ ->
        pattern = if is_list(pattern), do: pattern, else: [pattern]
        pattern_vars = TypeOperations.var_pattern(pattern, [expr_env[:type]])

        case pattern_vars do
          {:error, msg} -> {node, %{expr_env | state: :error, error_data: error_message(msg, meta)}}
          _ -> {node, %{expr_env | variable_ctx: Map.merge(expr_env[:variable_ctx], pattern_vars || %{})}}
        end
    end
  end

  # Variables
  def typecheck({x, meta, arg}, env) when is_atom(arg) do
    # Logger.debug("Typechecking: Variable #{inspect(x)} with type #{inspect(env[:variable_ctx][x])}")
    node = {x, meta, arg}

    if Map.has_key?(env[:variable_ctx], x) do
      {node, %{env | type: env[:variable_ctx][x]}}
    else
      {node, %{env | state: :error, error_data: error_message("Variable #{x} was not found", meta)}}
    end
  end

  # Case
  def typecheck({:case, meta, [expr, body | _]}, env) do
    # body contains [do: [ {:->, _, [ [ when/condition ], work ]}, other_cases... ] ]
    node = {:case, meta, []}
    cases = process_cases(body[:do])

    {_expr_ast, expr_env} = Macro.prewalk(expr, env, &typecheck/2)

    result =
      case expr_env[:state] do
        :error ->
          {:error, {:inner_error, expr_env[:error_data]}}

        _ ->
          # Get label, parameters and remaining ast from the source ast
          all_cases_result =
            Enum.map(cases, fn {lhs, rhs} ->
              pattern_vars = TypeOperations.var_pattern([lhs], [expr_env[:type]]) || %{}

              case pattern_vars do
                {:error, msg} ->
                  {:error, msg}

                _ ->
                  env = %{env | variable_ctx: Map.merge(env[:variable_ctx], pattern_vars)}

                  {_case_ast, case_env} = Macro.prewalk(rhs, env, &typecheck/2)

                  case case_env[:state] do
                    :error -> {:error, case_env[:error_data]}
                    _ -> case_env
                  end
              end
            end)

          process_cases_result(all_cases_result)
      end

    case result do
      {:error, _} = error -> {node, append_error(env, error, meta)}
      _ -> {node, %{env | session_type: result[:session_type], type: result[:type]}}
    end
  end

  # Send Function
  def typecheck({{:., _meta1, [:erlang, :send]}, meta2, [send_destination, send_body | _]}, env) do
    # Logger.debug("Typechecking: Erlang send")

    node = {nil, meta2, []}

    {_ast1, send_destination_env} = Macro.prewalk(send_destination, env, &typecheck/2)
    {_ast2, send_body_env} = Macro.prewalk(send_body, env, &typecheck/2)

    try do
      if send_destination_env[:state] == :error do
        throw({:error, send_destination_env[:error_data]})
      end

      if send_body_env[:state] == :error do
        throw({:error, send_body_env[:error_data]})
      end

      if TypeOperations.equal?(send_destination_env[:type], :pid) == false do
        throw({:error, "Expected pid in send statement, but found #{inspect(send_destination_env[:type])}"})
      end

      case send_body_env[:type] do
        {:tuple, _} -> :ok
        _ -> throw({:error, "Expected a tuple in send statement containing {:label, ...}"})
      end

      {:tuple, [label_type | parameter_types]} = send_body_env[:type]

      [label | parameters] = tuple_to_list(send_body)

      if TypeOperations.equal?(label_type, :atom) == false do
        throw({:error, "First item in tuple should be a literal/atom"})
      end

      # Unfold if session type starts with rec X.
      session_type = ST.unfold(env[:session_type])

      %GST.Send{label: expected_label, types: expected_types, next: remaining_session_types} =
        case session_type do
          %GST.Send{} = st ->
            st

          %GST.Choice{choices: choices} ->
            if choices[label] do
              choices[label]
            else
              throw(
                {:error,
                 "Cannot match send statement `#{Macro.to_string(send_body)}` " <>
                   "with #{ST.st_to_string_current(session_type)}"}
              )
            end

          x ->
            throw({:error, "Found a send/choice, but expected #{ST.st_to_string(x)}."})
        end

      if expected_label != label do
        throw({:error, "Expected send with label #{inspect(expected_label)} but found #{inspect(label)}."})
      end

      if length(expected_types) != length(parameter_types) do
        throw(
          {:error,
           "Session type payload length mismatch. Expected " <>
             "#{ST.st_to_string_current(session_type)} (length = " <>
             "#{length(expected_types)}), but found #{Macro.to_string(send_body)} " <>
             "(length = #{length(parameter_types)})."}
        )
      end

      if TypeOperations.equal?(parameter_types, expected_types) == false do
        throw(
          {:error,
           "Incorrect payload types. Expected " <>
             "#{ST.st_to_string_current(session_type)} " <>
             "but found #{Macro.to_string(parameters)} with type/s #{inspect(parameter_types)}"}
        )
      end

      {node, %{send_body_env | session_type: remaining_session_types}}
    catch
      {:error, message} ->
        {node, %{env | state: :error, error_data: error_message(message, meta2)}}

      x ->
        throw("Unknown error: " <> inspect(x))
    end
  end

  # Receive
  def typecheck({:receive, meta, [body | _]}, env) do
    # body contains [do: [ {:->, _, [ [ when/condition ], work ]}, other_cases... ] ]

    node = {:receive, meta, []}
    cases = process_cases(body[:do])

    try do
      # In case of one receive branch, it should match with a %ST.Recv{}
      # In case of more than one receive branch, it should match with a %ST.Branch{}
      # Unfold if session type starts with rec X.(...)
      session_type = ST.unfold(env[:session_type])

      branches_session_types =
        case session_type do
          %GST.Branch{branches: branches} -> branches
          %GST.Recv{label: label, types: types, next: next} -> %{label => %GST.Recv{label: label, types: types, next: next}}
          x -> throw({:error, "Found a receive/branch, but expected #{ST.st_to_string_current(x)}."})
        end

      # Each branch from the session type should have an equivalent branch in the receive cases
      if map_size(branches_session_types) != length(cases) do
        throw(
          {:error,
           "[in branch/receive] Mismatch in number of receive and & branches. " <>
             "Expected session type #{ST.st_to_string_current(session_type)}"}
        )
      end

      # Get label, parameters and remaining AST from the source AST
      all_branches_result =
        Enum.map(cases, fn {lhs, rhs} ->
          [head | _] = tuple_to_list(lhs)

          if branches_session_types[head] do
            %GST.Recv{types: expected_types, next: remaining_st} = branches_session_types[head]

            pattern_vars = TypeOperations.var_pattern([lhs], [{:tuple, [:atom] ++ expected_types}]) || %{}

            case pattern_vars do
              {:error, msg} ->
                {:error, msg}

              _ ->
                env = %{
                  env
                  | session_type: remaining_st,
                    variable_ctx: Map.merge(env[:variable_ctx], pattern_vars)
                }

                {_branch_ast, branch_env} = Macro.prewalk(rhs, env, &typecheck/2)

                case branch_env[:state] do
                  :error -> {:error, {:inner_error, branch_env[:error_data]}}
                  _ -> branch_env
                end
            end
          else
            throw({:error, "Receive branch with label #{inspect(head)} did not match session type"})
          end
        end)

      case process_cases_result(all_branches_result) do
        {:error, message} ->
          throw({:error, message})

        result ->
          {node, %{env | session_type: result[:session_type], type: result[:type]}}
      end
    catch
      {:error, _} = error ->
        {node, append_error(env, error, meta)}

      x ->
        throw("Unknown error: " <> inspect(x))
    end
  end

  # Hardcoded stuff (not ideal)
  def typecheck({{:., _meta1, [:erlang, :self]}, meta2, []}, env) do
    # Logger.debug("Typechecking: Erlang self")
    node = {nil, meta2, []}
    {node, %{env | type: :pid}}
  end

  def typecheck({{:., _meta, [IO, :puts]}, meta2, _}, env) do
    # Logger.debug("Typechecking: IO.puts")
    node = {nil, meta2, []}
    {node, %{env | type: :atom}}
  end

  def typecheck({{:., _meta, [IO, :gets]}, meta2, _}, env) do
    # Logger.debug("Typechecking: IO.gets")
    node = {nil, meta2, []}
    {node, %{env | type: :binary}}
  end

  def typecheck({{:., _meta, _call}, meta2, _}, env) do
    # Logger.debug("Typechecking: Remote function call (#{inspect(call)})")
    node = {nil, meta2, []}

    {node, %{env | state: :error, error_data: error_message("Remote functions not allowed.", meta2)}}
  end

  def typecheck({:., meta, _}, env) do
    node = {nil, meta, []}
    {node, env}
  end

  # Functions
  def typecheck({name, meta, args}, env) when is_list(args) do
    # Logger.debug("Typechecking: Function #{inspect(name)}")
    node = {name, meta, []}

    name_arity = {name, length(args)}

    try do
      function =
        case lookup_function(env[:functions], name_arity) do
          {:error, message} ->
            # Function does not exist in current module
            throw({:error, message})

          {:ok, function} ->
            function
        end

      if not function.types_known? do
        throw({:error, "Function #{name}/#{length(args)} has unknown return type. Use @spec to set parameter and return types."})
      end

      argument_types =
        for arg <- args do
          # Checks for parameter types
          {_ast, argument_env} = Macro.prewalk(arg, env, &typecheck/2)

          if argument_env[:state] == :error do
            throw({:error, argument_env[:error_data]})
          end

          argument_env[:type]
        end

      # Check argument types
      Enum.zip(function.param_types, argument_types)
      |> Enum.map(fn {expected, actual} ->
        unless TypeOperations.equal?(expected, actual) do
          throw(
            {:error,
             "Argument type error when calling #{name}/#{length(args)}: " <>
               "Expect #{TypeOperations.string(expected)} but found #{TypeOperations.string(actual)}"}
          )
        end
      end)

      # if TypeOperations.equal?(argument_env[:type])

      if env[:function_session_type_ctx][name_arity] do
        # Function with known session type (i.e. def with @session)
        function_session_type = env[:function_session_type_ctx][name_arity]
        expected_session_type = env[:session_type]

        if ST.equal?(function_session_type, expected_session_type) do
          {node, %{env | session_type: %GST.Terminate{}, type: function.return_type}}
        else
          throw(
            {:error,
             "Function #{name}/#{length(args)} has session type #{ST.st_to_string(function_session_type)} " <>
               "but was expecting #{ST.st_to_string_current(expected_session_type)}."}
          )
        end
      else
        # Function with unknown session type (i.e. defp)
        new_env = %{
          env
          | variable_ctx: %{},
            function_session_type_ctx: Map.merge(env[:function_session_type_ctx], %{name_arity => env[:session_type]})
        }

        new_env = session_typecheck_by_function(function, new_env)

        cond do
          new_env[:state] == :error ->
            throw({:error, new_env[:error_data]})

          new_env[:session_type] != %GST.Terminate{} ->
            throw({:error, "Function #{name}/#{length(args)} does not match the session type " <> ST.st_to_string(env[:session_type])})

          true ->
            {node, %{env | session_type: new_env[:session_type], type: new_env[:type]}}
        end
      end
    catch
      {:error, _} = error ->
        {node, append_error(env, error, meta)}

      x ->
        throw("Unknown error: " <> inspect(x))
    end
  end

  def typecheck(other, env) do
    # Logger.debug("Typechecking: other #{inspect(other)}")
    {other, env}
  end

  # Returns the lhs and rhs for all cases (i.e. lhs -> rhs)
  defp process_cases(cases) do
    Enum.map(cases, fn
      {:->, _, [[{:when, _, [var, _cond | _]}], rhs | _]} ->
        {var, rhs}

      {:->, _, [[lhs], rhs | _]} ->
        {lhs, rhs}
    end)
  end

  # Reduces a list of environments, ensuring that the type and session type are the same
  defp process_cases_result(all_cases) when is_list(all_cases) do
    Enum.reduce_while(all_cases, hd(all_cases), fn curr_case, acc ->
      case curr_case do
        {:error, message} ->
          {:halt, {:error, message}}

        _ ->
          # does not resolve to the same type necessarily
          # common_type = TypeOperations.equal?(curr_case[:type], acc[:type])

          # session type does not necessarily resolve to same type i.e. for now can have remaining recursive call or terminate
          # for now ignore terminates as those are successful and follow through with recursive call

          case curr_case[:session_type] do
            %GST.Call_Recurse{} = _st -> {:cont, curr_case}
            %GST.Terminate{} -> {:cont, acc}
          end

          # if common_type == false do
          #   {:halt,
          #    {:error,
          #     "Types #{inspect(curr_case[:type])} and #{inspect(acc[:type])} do not match. Different " <>
          #       "cases should have end up with the same type."}}
          # else
          #   if ST.equal?(curr_case[:session_type], acc[:session_type]) do
          #     {:cont, %{curr_case | type: curr_case[:type]}}
          #   else
          #     {:halt,
          #      {:error,
          #       "Mismatch in session type following the case: " <>
          #         "#{ST.st_to_string(curr_case[:session_type])} and " <>
          #         "#{ST.st_to_string(acc[:session_type])}"}}
          #   end
          # end
      end
    end)
  end

  defp append_error(env, {:error, {:inner_error, message}}, _meta) do
    %{env | state: :error, error_data: message}
  end

  defp append_error(env, {:error, message}, meta) do
    %{env | state: :error, error_data: error_message(message, meta)}
  end

  defp process_binary_operations(node, meta, operator, arg1, arg2, allowed_type, any_type, is_comparison, env) do
    {_op1_ast, op1_env} = Macro.prewalk(arg1, env, &typecheck/2)
    {_op2_ast, op2_env} = Macro.prewalk(arg2, env, &typecheck/2)

    try do
      if op1_env[:state] == :error do
        throw({:error, {:inner_error, op1_env[:error_data]}})
      end

      if op2_env[:state] == :error do
        throw({:error, {:inner_error, op2_env[:error_data]}})
      end

      cond do
        is_comparison ->
          {node,
           %{
             op1_env
             | type: :boolean,
               variable_ctx: Map.merge(op1_env[:variable_ctx], op2_env[:variable_ctx] || %{})
           }}

        operator == :| ->
          same_type = TypeOperations.equal?({:list, op1_env[:type]}, op2_env[:type])

          if same_type do
            {node,
             %{
               op1_env
               | type: {:list, op1_env[:type]},
                 variable_ctx: Map.merge(op1_env[:variable_ctx], op2_env[:variable_ctx] || %{})
             }}
          else
            {node,
             %{
               op1_env
               | state: :error,
                 error_data:
                   error_message(
                     "Operator type problem in [a | b]: b should be a list of the type of a. Found " <>
                       "#{TypeOperations.string(op1_env[:type])}, #{TypeOperations.string(op2_env[:type])}",
                     meta
                   )
             }}
          end

        true ->
          same_type = TypeOperations.equal?(op1_env[:type], op2_env[:type])

          if same_type == false do
            {node,
             %{
               op1_env
               | state: :error,
                 error_data:
                   error_message(
                     "Operator type problem in #{Atom.to_string(operator)}: #{TypeOperations.string(op1_env[:type])}, " <>
                       "#{TypeOperations.string(op2_env[:type])} are not of the same type",
                     meta
                   )
             }}
          else
            if any_type || TypeOperations.equal?(op1_env[:type], allowed_type) do
              {node,
               %{
                 op1_env
                 | type: op1_env[:type],
                   variable_ctx: Map.merge(op1_env[:variable_ctx], op2_env[:variable_ctx] || %{})
               }}
            else
              throw(
                {:error,
                 "Operator type problem in #{Atom.to_string(operator)}: #{TypeOperations.string(op1_env[:type])}, " <>
                   "#{TypeOperations.string(op2_env[:type])} is not of type #{inspect(allowed_type)}"}
              )
            end
          end
      end
    catch
      {:error, _} = error ->
        {node, append_error(env, error, meta)}

      x ->
        throw("Unknown error: " <> inspect(x))
    end
  end

  defp process_unary_operations(node, meta, arg1, expected_type, env) do
    {_op1_ast, op1_env} = Macro.prewalk(arg1, env, &typecheck/2)

    case op1_env[:state] do
      :error ->
        {node, op1_env}

      _ ->
        same_type = TypeOperations.equal?(op1_env[:type], expected_type)

        if same_type do
          {node, op1_env}
        else
          {node,
           %{
             op1_env
             | state: :error,
               error_data:
                 error_message(
                   "Type problem: Found expression of type #{inspect(op1_env[:type])} but expected a #{inspect(expected_type)}",
                   meta
                 )
           }}
        end
    end
  end

  defp tuple_to_list({arg1, arg2}) do
    [arg1, arg2]
  end

  defp tuple_to_list({:{}, _, args}) do
    args
  end

  defp remove_nils(list) do
    Enum.filter(
      list,
      fn
        {nil, _} -> false
        _ -> true
      end
    )
  end

  defp error_message(message, meta) do
    line =
      if meta[:line] do
        "[Line #{meta[:line]}] "
      else
        ""
      end

    line <> message
  end

  defp lookup_function(all_functions, {name, arity}) do
    try do
      {:ok, lookup_function!(all_functions, {name, arity})}
    catch
      {:error, x} -> {:error, x}
    end
  end

  defp lookup_function!(all_functions, {name, arity}) do
    if all_functions[{name, arity}] do
      all_functions[{name, arity}]
    else
      # Function does not exist in current module
      throw({:error, "Function #{name}/#{arity} was not found in the current module."})
    end
  end
end
