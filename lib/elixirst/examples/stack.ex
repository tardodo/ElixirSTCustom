defmodule Examples.Stack do

  use ElixirST

  use GenServer

  # @global_session "gS = &{push(number).{reply, string, [number]}.gS,
  #                         pop().{reply, number, [number]}.gS}"

  # @global_session "gS = push(number).#reply().rec X.(&{push(number).#reply(string, [number]).X,
  #                                                       pop().#reply(number, [number]).X})"

  @global_session "gS = &{push(number).#reply(string, [number]).gS,
                          pop().#reply(number, [number]).gS}"


  # Client

  def start_link(default) when is_list(default) do
    GenServer.start_link(__MODULE__, default)
  end

  # def push(pid, element) do
  #   GenServer.cast(pid, {:push, element})
  # end

  def push(pid, element) do
    GenServer.call(pid, {:push, element})
  end

  def pop(pid) do
    GenServer.call(pid, {:pop})
  end

  # Server (callbacks)

  @impl true
  def init(stack) do
    {:ok, stack}
  end


  # @impl true
  # # @session "X = ?push(number).X"
  # @spec handle_call({atom(), number}, any, [number]) :: {:reply, string, [number]}
  # def handle_call({:push, element},_from, state) do
  #   {:reply, "pushed", [element | state]}
  # end

  @impl true
  # @session "X = ?push(number).X"
  @spec handle_call({:push, number}, any, [number]) :: {:reply, string, [number]}
  def handle_call({:push, element},_from, state) do
    {:reply, "pushed", [element | state]}
  end

  @impl true
  @spec handle_call({:pop}, any, [number]) :: {:reply, number, [number]}
  def handle_call({:pop}, _from, [head | tail]) do
    {:reply, head, tail}
  end

  # @impl true
  # def handle_call(req, _from, state) do
  #   case req do
  #     {:push, element} -> {:reply, "pushed", [element | state]}
  #     :pop -> [head | tail] = state; {:reply, head, tail}
  #   end
  # end

  # @impl true
  # # @session "X = &{?push(number).!reply(string)}"
  # @spec handle_call(tuple, tuple, []) :: {:reply, String.t(), [number]}
  # def handle_call(req, _from, []) do
  #   case req do
  #     {:push, element} -> {:reply, "pushed", [element]}
  #     {_} -> {:reply, "error", []}
  #   end
  # end

  # @impl true
  # # @spec handle_call(tuple, tuple, tuple()) :: {:reply, String.t(), tuple()}
  # def handle_call(req, _from, {[], []}) do
  #   case req do
  #     {:push, element} -> {:reply, "pushed", {[:push] ,[element]}}
  #     {_} -> {:reply, "error", {[], []}}
  #   end
  # end

  # @impl true
  # @spec handle_call(tuple, tuple, tuple()) :: {:reply, String.t(), tuple()}
  # def handle_call(req, _from, state) do
  #   {history, server_state} = state
  #   [first | tail] = history
  #   case first do
  #     :push -> [sec | _] = tail; case sec do
  #        :push -> case req do
  #         {:push, element} -> {:reply, "pushed final", {history, [element | server_state]}}
  #         {:pop} when server_state != [] -> [x | xs] = server_state; {:reply, x, xs}
  #        end
  #        _ -> case req do
  #         {:push, element} -> {:reply, "pushed 2nd", {history ++ [:push] ,[element]}}
  #         _ -> {:reply, "error", []}
  #        end

  #       end
  #     _ -> case req do
  #        {:push, element} -> {:reply, "pushed base", {[:push] ,[element]}}
  #        _ -> {:reply, "error", []}
  #     end
  #     # _ -> {:reply, "error", []}
  #   end
  # end


  # CURRENT --------------------------------------------------
  # @impl true
  # @spec handle_call(tuple, tuple, tuple()) :: {:reply, String.t(), tuple()}
  # def handle_call(request, _from, state) do

  #   {history, server_state} = state

  #   case history do
  #     [{:push, _} | tail] ->
  #       case tail do
  #         [{:push, _} | _] ->
  #           case request do
  #             {:push, element} -> {:reply, "pushed final", {history, [element | server_state]}}
  #             {:pop} when server_state != [] -> [x | xs] = server_state; {:reply, x, {history, xs}}
  #             _ -> {:reply, "stack empty error", state}
  #           end
  #         _ ->
  #           case request do
  #             {:push, element} -> {:reply, "pushed 2nd", {history ++ [request] ,[element | server_state]}}
  #             _ -> {:reply, "error", state}
  #           end
  #       end
  #     _ ->
  #       case request do
  #         {:push, element} -> {:reply, "pushed base", {request ,[element]}}
  #         _ -> {:reply, "error", state}
  #       end
  #   end
  # end

  # -------------------------------------------------------------------

  # @impl true
  # def handle_call(req, _from, state) do
  #   case req do
  #     {:push, element} -> {:reply, "pushed", [element | state]}
  #     {:pop} when state != [] -> [head | tail] = state; {:reply, head, tail}
  #   end
  # end
  # |> IO.inspect

  # @impl true
  # def handle_call(req, _from, state) do
  #   case req do
  #     {:push, element} -> {:reply, "pushed", [element | state]}
  #     {:pop} when state != [] -> [head | tail] = state; {:reply, head, tail}
  #   end
  # end


  # @impl true
  # def handle_cast({:push, element}, state) do
  #   {:noreply, [element | state]}
  # end

  # def historyNav(hist) do
  #   [x | xs] = hist
  #   case x do
  #      :push -> [x | xs]; case  do
  #        ->

  #      end

  #      _ -> {:error}

  #   end

  # end


end
