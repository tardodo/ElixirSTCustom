defmodule Examples.Stack do

  use ElixirST

  use GenServer




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
  # def handle_call(:pop, _from, [head | tail]) do
  #   {:reply, head, tail}
  # end

  # @impl true
  # def handle_call(req, _from, state) do
  #   case req do
  #     {:push, element} -> {:reply, "pushed", [element | state]}
  #     :pop -> [head | tail] = state; {:reply, head, tail}
  #   end
  # end
  @impl true
  # @session "X = &{?push(number).!reply(string)}"
  @spec handle_call(tuple, tuple, []) :: {:reply, String.t(), [number]}
  def handle_call(req, _from, []) do
    case req do
      {:push, element} -> {:reply, "pushed", [element]}
      {_} -> {:reply, "error", []}
    end
  end



  # @impl true
  # def handle_call(req, _from, state) do
  #   case req do
  #     {:push, element} -> {:reply, "pushed", [element | state]}
  #     {:pop} when state != [] -> [head | tail] = state; {:reply, head, tail}
  #   end
  # end
  # |> IO.inspect


  # @impl true
  # def handle_cast({:push, element}, state) do
  #   {:noreply, [element | state]}
  # end
end
