defmodule Examples.StackMixed do

  use ElixirST

  use GenServer

  # @global_session "gS = &{push(number).{noreply, [number]}.gS,
  #                         pop().{reply, number, [number]}.gS}"

  # @global_session "gS = push(number).#reply().rec X.(&{push(number).#reply(binary, [number]).X,
  #                                                       pop().#reply(number, [number]).X})"

  # @global_session "gS = &{push(number).#noreply().gS,
  #                         pop().+{#stop(binary, number),
  #                                 #reply(number).gS}}"

  # @global_session "gS = &{push(number).#noreply([number]).gS}"

  @global_session "S = &{push(number).#noreply().rec X.(&{push(number).#noreply().rec Y.(&{push(number).#noreply().X}),
                                                              pop().+{#stop(binary, number),
                                                                       #reply(number).S}})}"


  # Client

  def start_link(default) when is_list(default) do
    GenServer.start_link(__MODULE__, default)
  end

  def runner() do
    {_, pid}= start_link([])
    push(pid, 5)
    push(pid, 6)
    pop(pid)


  end

  def push(pid, element) do
    GenServer.cast(pid, {:push, element})
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

  @impl true
  def handle_cast(req, state) do
    # fetch curr state
    st_state = ElixirST.StateTable.fetchCurrentState(__MODULE__)
    response = handle_cast(st_state, req, state)
    ElixirST.StateTable.transitionState(__MODULE__, st_state, req, response)

    response

  end


  @spec handle_cast(atom, {:push, number}, [number]) :: {:noreply, [number]}
  def handle_cast(_,{:push, element}, state) do
    {:noreply, [element | state]}
  end


  @spec handle_call(atom, {:pop}, any, [number]) :: {:reply, number, [number]}
  def handle_call(:X,{:pop}, _from, state) do

    if state == [] do
      {:stop, "Illegal POP", 0, []}
    else
      [head | tail] = state
      {:reply, head, tail}
    end
  end


end
