defmodule ElixirST.ParserGlobal do
  @moduledoc """
    Parses an input string to session types (as Elixir data).
  """
  alias ElixirST.GST
  # alias ElixirST.ST
  # require ST
  require GST

  @typedoc false
  # @type session_type :: ST.session_type()
  @type global_session_type :: GST.global_session_type()
  @typep session_type_tuple :: GST.session_type_tuple()
  # @typep session_type_tuple() :: ST.session_type_tuple()
  # @typep label :: ST.label()
  @typep label :: GST.label()

  @doc """
  Parses a string into a session type data structure

  ## Example
      iex> s = "rec Y.(+{!Hello(number, [{boolean, atom}]).Y, !Ok()})"
      ...> session_type = ElixirST.Parser.parse(s)
      ...> ElixirST.ST.st_to_string(session_type)
      "rec Y.(+{!Hello(number, [{boolean, atom}]).Y, !Ok()})"
  """
  # @spec parse(bitstring() | charlist()) :: session_type()
  # def parse(string) when is_bitstring(string) do
  #   st =
  #     string
  #     |> String.to_charlist()
  #     |> parse()

  #   validate!(st)
  #   st
  # end

  # def parse(string) do
  #   with {:ok, tokens, _} <- lexer(string) do
  #     if tokens == [] do
  #       # Empty input
  #       %ST.Terminate{}
  #     else
  #       case :parser.parse(tokens) do
  #         {:ok, session_type} ->
  #           convert_to_structs(session_type, [])

  #         {:error, errors} ->
  #           throw("Error while parsing session type #{inspect(string)}: " <> inspect(errors))
  #       end
  #     end
  #   else
  #     {:error, {_line, :lexer, error}, 1} ->
  #       # todo: cuter error message needed
  #       throw("Error in syntax of the session type " <> inspect(string) <> ". Found " <> inspect(error))
  #       []
  #   end
  # end

  defp lexer(string) do
    # IO.inspect tokens
    :lexer.string(string)
  end

  # Parse Global Session Type
  # Lexer remains the same so far

  @spec parseGlobal(bitstring() | charlist()) :: global_session_type()
  def parseGlobal(string) when is_bitstring(string) do
    gst =
      string
      |> String.to_charlist()
      |> parseGlobal()

    # validate!(gst)
    gst
  end

  def parseGlobal(string) do
    with {:ok, tokens, _} <- lexer(string) do
      if tokens == [] do
        # Empty input
        %GST.Terminate{}
      else
        case :parserGlobal.parse(tokens) do
          {:ok, session_type} ->
            convert_to_structs(session_type, [])

          {:error, errors} ->
            throw("Error while parsing session type #{inspect(string)}: " <> inspect(errors))
        end
      end
    else
      {:error, {_line, :lexer, error}, 1} ->
        # todo: cuter error message needed
        throw("Error in syntax of the session type " <> inspect(string) <> ". Found " <> inspect(error))
        []
    end
  end


  # Convert session types from Erlang records to Elixir Structs.
  # Throws error in case of branches/choices with same labels, or
  # if the types are not valid.
  @spec convert_to_structs(
          # should be { , , [atom], }
          {:funcall, atom, any, session_type_tuple()}
          | {:choice, [session_type_tuple()]}
          | {:branch, [session_type_tuple()]}
          | {:call, atom}
          | {:recurse, atom, session_type_tuple(), boolean()}
          | {:return, atom, any, session_type_tuple()}
          | {:terminate},
          [label()]
        ) :: global_session_type()
  defp convert_to_structs(session_type, recurse_var)

  defp convert_to_structs({:terminate}, _recurse_var) do
    %GST.Terminate{}
  end

  defp convert_to_structs({:funcall, label, types, next}, recurse_var) do
    checked_types = Enum.map(types, &ElixirST.TypeOperations.valid_type/1)

    Enum.each(checked_types, fn
      {:error, incorrect_types} -> throw("Invalid type/s: #{inspect(incorrect_types)}")
      _ -> :ok
    end)

    # follows = convert_to_structs(next, recurse_var)
    # case follows do
    #   %GST.Return{label: _, types: _, next: _} -> %GST.FunCall{label: label, types: checked_types, next: follows}
    #   # %GST.Choice{choices:  }
    #   _ -> throw("Function Call #{label} must be followed by a return")

    %GST.FunCall{label: label, types: checked_types, next: convert_to_structs(next, recurse_var)}
    # end


    # %GST.FunCall{label: label, types: checked_types, next: convert_to_structs(next, recurse_var)}


  end

  defp convert_to_structs({:return, label, types, next}, recurse_var) do
    checked_types = Enum.map(types, &ElixirST.TypeOperations.valid_type/1)

    Enum.each(checked_types, fn
      {:error, incorrect_types} -> throw("Invalid type/s: #{inspect(incorrect_types)}")
      _ -> :ok
    end)


    %GST.Return{label: label, types: checked_types, next: convert_to_structs(next, recurse_var)}


  end

  defp convert_to_structs({:choice, choices}, recurse_var) do
    %GST.Choice{
      choices:
        Enum.reduce(
          choices,
          %{},
          fn choice, map ->
            converted_st = convert_to_structs(choice, recurse_var)
            label = label(converted_st)

            if Map.has_key?(map, label) do
              throw("Cannot insert multiple choices with same label: #{label}.")
            else
              Map.put(map, label, converted_st)
            end
          end
        )
    }
  end

  defp convert_to_structs({:branch, branches}, recurse_var) do
    %GST.Branch{
      branches:
        Enum.reduce(
          branches,
          %{},
          fn branch, map ->
            converted_st = convert_to_structs(branch, recurse_var)
            label = label(converted_st)

            if Map.has_key?(map, label) do
              throw("Cannot insert multiple branches with same label: #{label}.")
            else
              Map.put(map, label, converted_st)
            end
          end
        )
    }
  end

  defp convert_to_structs({:recurse, label, body, outer_recurse}, recurse_var) do
    # if label in recurse_var do
    #   throw("Cannot have multiple recursions with same variable: #{label}.")
    # end

    %GST.Recurse{label: label, body: convert_to_structs(body, [label | recurse_var]), outer_recurse: outer_recurse}
  end

  defp convert_to_structs({:call, label}, _recurse_var) do
    %GST.Call_Recurse{label: label}
  end

  defp label(%GST.FunCall{label: label}) do
    label
  end

  defp label(%GST.Return{label: label}) do
    label
  end

  defp label(_) do
    throw("Following a branch/choice, a function call or return statement is required.")
  end

  # Performs validations on the session type.
#   @spec validate!(session_type()) :: boolean()
#   defp validate!(session_type)

#   defp validate!(%ST.Send{next: next}) do
#     validate!(next)
#   end

#   defp validate!(%ST.Recv{next: next}) do
#     validate!(next)
#   end

#   defp validate!(%ST.Choice{choices: choices}) do
#     res =
#       Enum.map(
#         choices,
#         fn
#           {_label, %ST.Send{next: next}} ->
#             validate!(next)

#           {_, other} ->
#             throw("Session type parsing validation error: Each branch needs a send as the first statement: #{ST.st_to_string(other)}.")

#             false

#           _ ->
#             throw("BAD - check")
#         end
#       )

#     # Return false if one (or more) false are found
#     Enum.find(res, true, fn x -> !x end)
#   end

#   defp validate!(%ST.Branch{branches: branches}) do
#     res =
#       Enum.map(
#         branches,
#         fn
#           {_label, %ST.Recv{next: next}} ->
#             validate!(next)

#           {_, other} ->
#             throw("Session type parsing validation error: Each branch needs a receive as the first statement: #{ST.st_to_string(other)}.")

#             false

#           _ ->
#             throw("BAD - check")
#         end
#       )

#     if false in res do
#       false
#     else
#       true
#     end
#   end

#   defp validate!(%ST.Recurse{body: body} = st) do
#     case body do
#       %ST.Recurse{} ->
#         throw("It is unnecessary to having multiple recursions following each other: '#{ST.st_to_string(st)}'")

#       _ ->
#         validate!(body)
#     end
#   end

#   defp validate!(%ST.Call_Recurse{}) do
#     true
#   end

#   defp validate!(%ST.Terminate{}) do
#     true
#   end

#   defp validate!(x) do
#     throw("Validation problem. Unknown input: #{inspect(x)}")
#     false
#   end
end
