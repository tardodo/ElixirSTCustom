defmodule Examples.StackMixed do

  use ElixirST

  use GenServer

  # @global_session "gS = &{push(number).{noreply, [number]}.gS,
  #                         pop().{reply, number, [number]}.gS}"

  # @global_session "gS = push(number).#reply().rec X.(&{push(number).#reply(binary, [number]).X,
  #                                                       pop().#reply(number, [number]).X})"

  @global_session "gS = &{push(number).#noreply().gS,
                          pop().#reply(number).gS}"

  # @global_session "gS = &{push(number).#noreply([number]).gS}"


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
    {:ok, stack}
  end


  @impl true
  @spec handle_cast({:push, number}, [number]) :: {:noreply, [number]}
  def handle_cast({:push, element}, state) do
    {:noreply, [element | state]}
  end

  @impl true
  @spec handle_call({:pop}, any, [number]) :: {:reply, number, [number]}
  def handle_call({:pop}, _from, [head | tail]) do
    {:reply, head, tail}
  end


end
