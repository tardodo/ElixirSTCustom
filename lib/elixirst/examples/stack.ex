defmodule Examples.Stack do

  use ElixirST

  use GenServer

  # @global_session "gS = &{push(number).{reply, binary, [number]}.gS,
  #                         pop().{reply, number, [number]}.gS}"

  # @global_session "gS = push(number).#reply().rec X.(&{push(number).#reply(binary, [number]).X,
  #                                                       pop().#reply(number, [number]).X})"

  # @global_session "gS = &{push(number).#reply(binary).gS,
  #                         pop().#reply(number).gS}"

  # @global_session "X = &{push(number).#reply(binary).X,
  #                         pop().+{#stop(binary, binary),
  #                                 #reply(number).X}}"

  @global_session "S = &{push(number).#reply(binary).rec X.(&{push(number).#reply(binary).X,
                                                              pop().+{#stop(binary, binary),
                                                                       #reply(number).S}})}"
  # @global_session "gS = &{push(number).#reply(binary, [number]).gS}"


  # Client

  def start_link(default) when is_list(default) do
    GenServer.start_link(__MODULE__, default)
  end

  # def runner() do
  #   {_, pid}= start_link([])
  #   push(pid, 5)
  #   push(pid, 6)
  #   pop(pid)


  # end

  def push(pid, element) do
    GenServer.call(pid, {:push, element})
    # ElixirST.stateTrans(__MODULE__)
  end

  def pop(pid) do
    GenServer.call(pid, {:pop})
  end

  # Server (callbacks)

  @impl true
  def init(stack) do
    ElixirST.StateTable.createTable(__MODULE__)

    {:ok, stack}
  end

  @impl true
  def handle_call(req, from, state) do
    # fetch curr state
    st_state = ElixirST.StateTable.fetchCurrentState(__MODULE__)
    response = handle_call(st_state, req, from, state)
    ElixirST.StateTable.transitionState(__MODULE__, st_state, req, response)

    response

  end

  # @impl true
  # @spec handle_call(:S, {:push, number}, any, [number]) :: {:reply, binary, [number]}
  # defp handle_call(:S, {:push, element},_from, state) do

  #   {:reply, "pushed", [element | state]}
  # end

  @spec handle_call(atom, {:push, number}, any, [number]) :: {:reply, binary, [number]}
  defp handle_call(_, {:push, element},_from, state) do

    {:reply, "pushed", [element | state]}
  end

  # @impl true
  @spec handle_call(:X, {:pop}, any, [number]) :: {:reply, number, [number]}
  defp handle_call(:X, {:pop}, _from, state) do

    if(state == []) do
      {:stop, "Invalid POP", "Empty Stack",[]}
    else
      [head | tail ] = state
      {:reply, head, tail}
    end
  end

  # @impl true
  # @spec handle_call({:push, number}, any, [number]) :: {:reply, binary, [number]}
  # def handle_call({:push, element},_from, state) do

  #   {:reply, "pushed", [element | state]}
  # end

  # @impl true
  # @spec handle_call({:pop}, any, [number]) :: {:reply, number, [number]}
  # def handle_call({:pop}, _from, state) do

  #   if(state == []) do
  #     {:stop, "Invalid POP", "Empty Stack",[]}
  #   else
  #     [head | tail ] = state
  #     {:reply, head, tail}
  #   end
  # end


end
