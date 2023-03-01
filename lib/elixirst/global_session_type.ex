defmodule ElixirST.GST do
  @moduledoc """
  Manipulate Session Type (ST) data.

  Session type definitions:
      ! = send
      ? = receive
      & = branch (or external choice)
      + = (internal) choice

  Session types accept the following grammar:

      S =
          !label(types, ...).S
        | ?label(types, ...).S
        | &{?label(types, ...).S, ...}
        | +{!label(types, ...).S, ...}
        | rec X.(S)
        | X
        | end

  Note: The session type `end` is optional, therefore `!Hello()` and `!Hello().end` are equivalent.
  `X` refers to to a variable which can be called later in a recursion operation.
  `rec X.(S)` refers to recursion, or looping - when `X` is called, it is replaced with the whole session type
  `rec X.(S)`.

  Some session types examples:

      !Hello()                           # Sends {:Hello}

      ?Ping(number)                      # Receives {:Ping, value}, where values has to be a number

      &{?Option1().!Hello(), ?Option2()} # Receive either {:Option1} or {:Option2}. If it
                                         # receives the former, then it sends {:Hello}. If it
                                         # receives {:Option2}, then it terminates.

      rec X.(&{?Stop().end, ?Retry().X}) # The actor is able to receive multiple {:Retry},
                                         # and terminates when it receives {:Stop}.


  Internal representation of session types take the form of the following structs:
  - `%Send{label, types, next}`
  - `%Recv{label, types, next}`
  - `%Choice{choices}`
  - `%Branch{branches}`
  - `%Recurse{label, body}`
  - `%Call_Recurse{label}`
  - `%Terminate{}`

  The labels and types are of type `t:label/0` and `t:types/0`, respectively. `next`, `choices`, `branches` and `body` have the type
  `t:session_type/0`.

  ### Parser

  Parses an input string to session types (as Elixir data).

  #### Simple example

      iex> s = "!Hello(Number)"
      ...> ElixirST.ST.string_to_st(s)
      %ElixirST.ST.Send{label: :Hello, next: %ElixirST.ST.Terminate{}, types: [:number]}

  #### Another example

      iex> s = "rec X.(&{?Ping().!Pong().X, ?Quit().end})"
      ...> ElixirST.ST.string_to_st(s)
      %ElixirST.ST.Recurse{
        label: :X,
        body: %ElixirST.ST.Branch{
          branches: %{
            Ping: %ElixirST.ST.Recv{
              label: :Ping,
              next: %ElixirST.ST.Send{label: :Pong, next: %ElixirST.ST.Call_Recurse{label: :X}, types: []},
              types: []
            },
            Quit: %ElixirST.ST.Recv{label: :Quit, next: %ElixirST.ST.Terminate{}, types: []}
          }
        }
      }

  """
  alias ElixirST.GST

  @typedoc """
  A session type list of session operations.
  """
  @type session_type() ::
          %GST.Send{label: label(), types: types(), next: session_type()}
          | %GST.Recv{label: label(), types: types(), next: session_type()}
          | %GST.Choice{choices: %{label() => session_type()}}
          | %GST.Branch{branches: %{label() => session_type()}}
          | %GST.Recurse{label: label(), body: session_type()}
          | %GST.Call_Recurse{label: label()}
          | %GST.Terminate{}
          # add function call
          # | %GST.FunCall{label: label(), types: types(), next: session_type()}

  # global session type with functions
  @type global_session_type() ::
          %GST.FunCall{label: label(), types: types(), next: global_session_type()}
          | %GST.Branch{branches: %{label() => global_session_type()}}
          | %GST.Recurse{label: label(), body: global_session_type()}
          | %GST.Call_Recurse{label: label()}
          | %GST.Terminate{}
          | %GST.Return{label: label(), types: types(), next: global_session_type()}

  @typedoc """
  Session types when stored as tuples. Useful for when converting from Erlang records.
  """
  @type session_type_tuple() ::
          {:send, atom, [atom], session_type_tuple()}
          | {:recv, atom, [atom], session_type_tuple()}
          | {:choice, [session_type_tuple]}
          | {:branch, [session_type_tuple]}
          | {:call, atom}
          | {:recurse, atom, session_type_tuple}
          | {:terminate}
          | {:funcall, atom, [atom], session_type_tuple()}
          | {:return, atom, [atom], session_type_tuple()}

  @typedoc """
  Label for sending/receiving statements. Should be of the form of an `atom`.
  """
  @type label() :: atom()

  @typedoc """
  Type for name and arity keys.
  """
  @type name_arity() :: {label(), non_neg_integer()}

  @typedoc """
  Native types accepted in the send/receive statements.
  E.g. !Ping(integer)
  """
  @type types() :: [
          :atom
          | :binary
          | :bitstring
          | :boolean
          | :exception
          | :float
          | :function
          | :integer
          | :list
          | :map
          | nil
          | :number
          | :pid
          | :port
          | :reference
          | :struct
          | :tuple
          | :string
        ]

  @typedoc """
  Abstract Syntax Tree (AST)
  """
  @type ast() :: Macro.t()

  defmodule Terminate do
    @moduledoc false
    defstruct []
    @type t :: %__MODULE__{}
  end

  defmodule Send do
    @moduledoc false
    @enforce_keys [:label]
    defstruct [:label, types: [], next: %GST.Terminate{}]

    @type session_type() :: GST.session_type()
    @type label() :: GST.label()
    @type types() :: GST.types()
    @type t :: %__MODULE__{label: label(), types: types(), next: session_type()}
  end

  defmodule Recv do
    @moduledoc false
    @enforce_keys [:label]
    defstruct [:label, types: [], next: %GST.Terminate{}]

    @type session_type() :: GST.session_type()
    @type label() :: GST.label()
    @type types() :: GST.types()
    @type t :: %__MODULE__{label: label(), types: types(), next: session_type()}
  end

  defmodule Choice do
    @moduledoc false
    @enforce_keys [:choices]
    defstruct [:choices]

    @type global_session_type() :: GST.global_session_type()
    @type label() :: GST.label()
    @type t :: %__MODULE__{choices: %{label() => global_session_type()}}
  end

  defmodule Branch do
    @moduledoc false
    @enforce_keys [:branches]
    defstruct [:branches]

    @type global_session_type() :: GST.global_session_type()
    @type label() :: GST.label()
    @type t :: %__MODULE__{branches: %{label() => global_session_type()}}
  end

  defmodule Recurse do
    @moduledoc false
    @enforce_keys [:label, :body]
    defstruct [:label, :body, outer_recurse: false]

    @type global_session_type() :: GST.global_session_type()
    @type label() :: GST.label()
    @type t :: %__MODULE__{label: label(), body: global_session_type(), outer_recurse: boolean()}
  end

  defmodule Call_Recurse do
    @moduledoc false
    @enforce_keys [:label]
    defstruct [:label]

    @type global_session_type() :: GST.global_session_type()
    @type label() :: GST.label()
    @type t :: %__MODULE__{label: label()}
  end

  defmodule Function do
    @moduledoc false

    @enforce_keys [:name]
    defstruct name: nil,
              arity: 0,
              def_p: :def,
              # List of bodies from different (pattern-matching) cases
              bodies: [],
              # Function meta
              meta: [],
              # Number of different patter-matching cases
              cases: 0,
              # List of function cases meta
              case_metas: [],
              # List (of list) of parameters
              parameters: [],
              # List (of list) of guards
              guards: [],
              types_known?: false,
              return_type: [],
              param_types: []

    # Structure of functions in Beam debug_info
    # {{name, arity}, :def_or_p, meta, [{meta, parameters, guards, body}, case2, ...]}

    @type label() :: GST.label()
    @type t :: %__MODULE__{
            name: label(),
            arity: non_neg_integer(),
            def_p: :def | :defp,
            bodies: [any()],
            meta: [any()],
            case_metas: [any()],
            parameters: [any()],
            guards: [any()],
            types_known?: boolean(),
            return_type: [any()],
            param_types: [any()]
          }
  end

  defmodule Module do
    @moduledoc false
    defstruct functions: [],
              function_st_context: %{},
              module_name: :"",
              file: "",
              relative_file: "",
              line: 1

    @type global_session_type() :: GST.global_session_type()
    @type label() :: GST.label()
    @type ast() :: GST.ast()
    @type func_name_arity() :: GST.name_arity()
    @type t :: %__MODULE__{
            functions: [GST.Function.t()],
            function_st_context: %{func_name_arity() => global_session_type()},
            module_name: atom(),
            file: String.t(),
            relative_file: String.t(),
            line: integer()
          }
  end

  # function call
  defmodule FunCall do
    @moduledoc false
    @enforce_keys [:label]
    defstruct [:label, types: [], next: %GST.Terminate{}]

    @type global_session_type() :: GST.global_session_type()
    @type label() :: GST.label()
    @type types() :: GST.types()
    @type t :: %__MODULE__{label: label(), types: types(), next: global_session_type()}
  end

  defmodule Return do
    @moduledoc false
    @enforce_keys [:label]
    defstruct [:label, types: [], next: %GST.Terminate{}]

    @type global_session_type() :: GST.global_session_type()
    @type label() :: GST.label()
    @type types() :: GST.types()
    @type t :: %__MODULE__{label: label(), types: types(), next: global_session_type()}
  end

  @doc """
  Converts s session type to a string. To do the opposite, use `string_to_st/1`.

  ## Examples
      iex> s = "rec x.(&{?Hello(number), ?Retry().X})"
      ...> st = ElixirST.ST.string_to_st(s)
      ...> ElixirST.ST.st_to_string(st)
      "rec x.(&{?Hello(number), ?Retry().X})"
  """
  # @spec st_to_string(session_type()) :: String.t()
  # def st_to_string(%GST.Terminate{}), do: "end"

  # def st_to_string(session_type) do
  #   st_to_string_internal(session_type)
  # end

  # defp st_to_string_internal(%GST.Send{label: label, types: types, next: next}) do
  #   types_string = Enum.map(types, &ElixirST.TypeOperations.string/1) |> Enum.join(", ")

  #   following_st = st_to_string_internal(next)

  #   if following_st != "" do
  #     "!#{label}(#{types_string}).#{following_st}"
  #   else
  #     "!#{label}(#{types_string})"
  #   end
  # end

  # defp st_to_string_internal(%GST.Recv{label: label, types: types, next: next}) do
  #   types_string = Enum.map(types, &ElixirST.TypeOperations.string/1) |> Enum.join(", ")

  #   following_st = st_to_string_internal(next)

  #   if following_st != "" do
  #     "?#{label}(#{types_string}).#{following_st}"
  #   else
  #     "?#{label}(#{types_string})"
  #   end
  # end

  # defp st_to_string_internal(%GST.Choice{choices: choices}) do
  #   v =
  #     Enum.map(choices, fn {_label, x} -> st_to_string_internal(x) end)
  #     |> Enum.join(", ")

  #   "+{#{v}}"
  # end

  # defp st_to_string_internal(%GST.Branch{branches: branches}) do
  #   v =
  #     Enum.map(branches, fn {_label, x} -> st_to_string_internal(x) end)
  #     |> Enum.join(", ")

  #   "&{#{v}}"
  # end

  # defp st_to_string_internal(%GST.Recurse{label: label, body: body, outer_recurse: outer_recurse}) do
  #   if outer_recurse do
  #     "#{label} = #{st_to_string_internal(body)}"
  #   else
  #     "rec #{label}.(#{st_to_string_internal(body)})"
  #   end
  # end

  # defp st_to_string_internal(%GST.Call_Recurse{label: label}) do
  #   "#{label}"
  # end

  # defp st_to_string_internal(%GST.Terminate{}) do
  #   ""
  # end

  @doc """
  Converts the current session type to a string. E.g. ?Hello().!hi() would return ?Hello() only.

  ## Examples
      iex> s = "?Hello(number).?Retry()"
      ...> st = ElixirST.ST.string_to_st(s)
      ...> ElixirST.ST.st_to_string_current(st)
      "?Hello(number)"
  """
  # @spec st_to_string_current(session_type()) :: String.t()
  # def st_to_string_current(%GST.Terminate{}), do: "end"

  # def st_to_string_current(session_type) do
  #   st_to_string_current_internal(session_type)
  # end

  # @spec st_to_string_current_internal(session_type()) :: String.t()
  # defp st_to_string_current_internal(session_type)

  # defp st_to_string_current_internal(%GST.Send{label: label, types: types}) do
  #   types_string = Enum.map(types, &ElixirST.TypeOperations.string/1) |> Enum.join(", ")

  #   "!#{label}(#{types_string})"
  # end

  # defp st_to_string_current_internal(%GST.Recv{label: label, types: types}) do
  #   types_string = Enum.map(types, &ElixirST.TypeOperations.string/1) |> Enum.join(", ")

  #   "?#{label}(#{types_string})"
  # end

  # defp st_to_string_current_internal(%GST.Choice{choices: choices}) do
  #   v =
  #     Enum.map(choices, fn {_, x} -> st_to_string_current_internal(x) end)
  #     |> Enum.map(fn x -> x <> "..." end)
  #     |> Enum.join(", ")

  #   "+{#{v}}"
  # end

  # defp st_to_string_current_internal(%GST.Branch{branches: branches}) do
  #   v =
  #     Enum.map(branches, fn {_, x} -> st_to_string_current_internal(x) end)
  #     |> Enum.map(fn x -> x <> "..." end)
  #     |> Enum.join(", ")

  #   "&{#{v}}"
  # end

  # defp st_to_string_current_internal(%GST.Recurse{label: label, body: body, outer_recurse: outer_recurse}) do
  #   if outer_recurse do
  #     "#{label} = #{st_to_string_current_internal(body)}"
  #   else
  #     "rec #{label}.(#{st_to_string_current_internal(body)})"
  #   end
  # end

  # defp st_to_string_current_internal(%GST.Call_Recurse{label: label}) do
  #   "#{label}"
  # end

  # defp st_to_string_current_internal(%GST.Terminate{}) do
  #   ""
  # end

  @doc """
  Converts a string to a session type. To do the opposite, use `st_to_string/1`.

  ## Examples
      iex> s = "?Ping().!Pong()"
      ...> ElixirST.ST.string_to_st(s)
      %ElixirST.ST.Recv{
        label: :Ping,
        next: %ElixirST.ST.Send{label: :Pong, next: %ElixirST.ST.Terminate{}, types: []},
        types: []
      }
  """
  @spec string_to_st(String.t()) :: global_session_type()
  def string_to_st(st_string) do
    ElixirST.ParserGlobal.parseGlobal(st_string)
  end

  @doc """
  Returns the dual of the given session type.

  ### Changes that are made:
  -  Receive <-> Send
  -  Branch  <-> Choice

  ## Examples
      iex> st_string = "!Ping(Number).?Pong(String)"
      ...> st = ElixirST.Parser.parse(st_string)
      ...> st_dual = ElixirST.ST.dual(st)
      ...> ElixirST.ST.st_to_string(st_dual)
      "?Ping(number).!Pong(string)"

  """
  # @spec dual(session_type()) :: session_type()
  # def dual(session_type)

  # def dual(%GST.Send{label: label, types: types, next: next}) do
  #   %GST.Recv{label: label, types: types, next: dual(next)}
  # end

  # def dual(%GST.Recv{label: label, types: types, next: next}) do
  #   %GST.Send{label: label, types: types, next: dual(next)}
  # end

  # def dual(%GST.Choice{choices: choices}) do
  #   %GST.Branch{
  #     branches:
  #       Enum.map(choices, fn {label, choice} -> {label, dual(choice)} end)
  #       |> Enum.into(%{})
  #   }
  # end

  # def dual(%GST.Branch{branches: branches}) do
  #   %GST.Choice{
  #     choices:
  #       Enum.map(branches, fn {label, branches} -> {label, dual(branches)} end)
  #       |> Enum.into(%{})
  #   }
  # end

  # def dual(%GST.Recurse{label: label, body: body}) do
  #   %GST.Recurse{label: label, body: dual(body)}
  # end

  # def dual(%GST.Call_Recurse{} = st) do
  #   st
  # end

  # def dual(%GST.Terminate{} = st) do
  #   st
  # end

  # # Checks if the two given session types are dual of each other
  # @spec dual?(session_type(), session_type()) :: boolean()
  # def dual?(session_type1, session_type2)

  # def dual?(
  #       %GST.Send{label: label, types: types, next: next1},
  #       %GST.Recv{label: label, types: types, next: next2}
  #     ) do
  #   dual?(next1, next2)
  # end

  # def dual?(%GST.Recv{} = a, %GST.Send{} = b) do
  #   dual?(b, a)
  # end

  # def dual?(%GST.Choice{choices: choices}, %GST.Branch{branches: branches}) do
  #   # %GST.Branch{branches: Enum.map(choices, fn choice -> dual?(choice) end)}
  #   labels_choices = Map.keys(choices) |> MapSet.new()
  #   labels_branches = Map.keys(branches) |> MapSet.new()

  #   # Check that all labels from the 'choice' are included in the 'branches'.
  #   check = MapSet.subset?(labels_choices, labels_branches)

  #   if check do
  #     Enum.map(labels_choices, fn label -> dual?(choices[label], branches[label]) end)
  #     # Finds in there are any 'false'
  #     |> Enum.find(true, fn x -> !x end)
  #   else
  #     false
  #   end
  # end

  # def dual?(%GST.Branch{} = a, %GST.Choice{} = b) do
  #   dual?(b, a)
  # end

  # def dual?(%GST.Choice{choices: choices}, %GST.Recv{} = recv) do
  #   if map_size(choices) > 1 do
  #     false
  #   else
  #     lhs = Map.values(choices)
  #     dual?(hd(lhs), recv)
  #   end
  # end

  # def dual?(%GST.Recv{} = a, %GST.Choice{} = b) do
  #   dual?(b, a)
  # end

  # def dual?(%GST.Branch{branches: branches}, %GST.Send{label: label} = send) do
  #   dual?(branches[label], send)
  # end

  # def dual?(%GST.Send{} = a, %GST.Branch{} = b) do
  #   dual?(b, a)
  # end

  # def dual?(%GST.Recurse{label: label, body: body1}, %GST.Recurse{label: label, body: body2}) do
  #   dual?(body1, body2)
  # end

  # def dual?(%GST.Call_Recurse{}, %GST.Call_Recurse{}) do
  #   true
  # end

  # def dual?(%GST.Terminate{}, %GST.Terminate{}) do
  #   true
  # end

  # def dual?(_, _) do
  #   false
  # end

  # Equality, takes into consideration that recursions with a different variable name are equal
  # Pattern matching with ST.session_type()
  # ! = +{l} and & = &{l}
  @spec equal?(session_type(), session_type()) :: boolean()
  def equal?(session_type1, session_type2) do
    equal?(session_type1, session_type2, %{})
  end

  @spec equal?(session_type(), session_type(), %{}) :: boolean()
  defp equal?(session_type, session_type, recurse_var_mapping)

  defp equal?(
         %GST.Send{label: label, types: types, next: next1},
         %GST.Send{label: label, types: types, next: next2},
         recurse_var_mapping
       ) do
    equal?(next1, next2, recurse_var_mapping)
  end

  defp equal?(
         %GST.Recv{label: label, types: types, next: next1},
         %GST.Recv{label: label, types: types, next: next2},
         recurse_var_mapping
       ) do
    equal?(next1, next2, recurse_var_mapping)
  end

  defp equal?(%GST.Choice{choices: choices1}, %GST.Choice{choices: choices2}, recurse_var_mapping) do
    # Sorting is done (automatically) by the map

    Enum.zip(Map.values(choices1), Map.values(choices2))
    |> Enum.reduce(
      true,
      fn
        {choice1, choice2}, acc ->
          acc and equal?(choice1, choice2, recurse_var_mapping)
      end
    )
  end

  defp equal?(
         %GST.Branch{branches: branches1},
         %GST.Branch{branches: branches2},
         recurse_var_mapping
       ) do
    # Sorting is done (automatically) by the map

    Enum.zip(Map.values(branches1), Map.values(branches2))
    |> Enum.reduce(
      true,
      fn
        {branche1, branche2}, acc ->
          acc and equal?(branche1, branche2, recurse_var_mapping)
      end
    )
  end

  defp equal?(
         %GST.Recurse{label: label1, body: body1},
         %GST.Recurse{label: label2, body: body2},
         recurse_var_mapping
       ) do
    equal?(body1, body2, Map.put(recurse_var_mapping, label1, label2))
  end

  defp equal?(
         %GST.Call_Recurse{label: label1},
         %GST.Call_Recurse{label: label2},
         recurse_var_mapping
       ) do
    case Map.fetch(recurse_var_mapping, label1) do
      {:ok, ^label2} ->
        true

      _ ->
        # In case of free var
        label1 == label2
    end
  end

  defp equal?(%GST.Terminate{}, %GST.Terminate{}, _recurse_var_mapping) do
    true
  end

  defp equal?(_, _, _) do
    false
  end

  @doc """
  Takes a session type (starting with a recursion, e.g. rec X.(...)) and outputs a single unfold of X.


  ## Examples
          iex> st = "rec X.(!A().X)"
          ...> session_type = ElixirST.ST.string_to_st(st)
          ...> unfolded = ElixirST.ST.unfold(session_type)
          ...> ElixirST.ST.st_to_string(unfolded)
          "!A().rec X.(!A().X)"
  """
  @spec unfold(session_type()) :: session_type()
  def unfold(%GST.Recurse{label: label, body: body} = rec) do
    unfold(body, label, rec)
  end

  def unfold(x) do
    x
  end

  @spec unfold(session_type(), label(), ST.Recurse.t()) :: session_type()
  defp unfold(%GST.Send{label: label_send, types: types, next: next}, label, rec) do
    %GST.Send{label: label_send, types: types, next: unfold(next, label, rec)}
  end

  defp unfold(%GST.Recv{label: label_recv, types: types, next: next}, label, rec) do
    %GST.Recv{label: label_recv, types: types, next: unfold(next, label, rec)}
  end

  defp unfold(%GST.Choice{choices: choices}, label, rec) do
    %GST.Choice{
      choices:
        Enum.map(choices, fn {l, choice} -> {l, unfold(choice, label, rec)} end)
        |> Enum.into(%{})
    }
  end

  defp unfold(%GST.Branch{branches: branches}, label, rec) do
    %GST.Branch{
      branches:
        Enum.map(branches, fn {l, branch} -> {l, unfold(branch, label, rec)} end)
        |> Enum.into(%{})
    }
  end

  defp unfold(%GST.Recurse{label: diff_label, body: body}, label, rec) do
    %GST.Recurse{label: diff_label, body: unfold(body, label, rec)}
  end

  defp unfold(%GST.Call_Recurse{label: label}, label, rec) do
    rec
  end

  defp unfold(%GST.Call_Recurse{label: diff_label}, _label, _rec) do
    %GST.Call_Recurse{label: diff_label}
  end

  defp unfold(%GST.Terminate{} = st, _label, _rec) do
    st
  end
end
