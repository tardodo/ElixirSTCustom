defmodule Examples.GenCalc do
  use ElixirST
  use GenServer


  @global_session "X = &{add(number, number).#reply(number).X,
                          sub(number, number).#reply(number).X,
                          mult(number, number).#reply(number).X,
                          div(number, number).+{#reply(number).X,
                                                #stop(binary, binary)
                                                }
                        }"

  def start_link(default) when is_list(default) do
    GenServer.start_link(__MODULE__, default)
  end

  def add(pid, a, b) do
    GenServer.call(pid, {:add, a, b})
  end

  def sub(pid, a, b) do
    GenServer.call(pid, {:sub, a, b})
  end

  def mult(pid, a, b) do
    GenServer.call(pid, {:mult, a, b})
  end

  def div(pid, a, b) do
    GenServer.call(pid, {:div, a, b})
  end

  @impl true
  def init(calc) do
    {:ok, calc}
  end

  @impl true
  @spec handle_call({:add, number, number}, any, [number]) :: {:reply, number, [number]}
  def handle_call({:add, a, b}, _from, state) do
    ans = a + b
    {:reply, ans, [ans | state]}
  end

  @impl true
  @spec handle_call({:sub, number, number}, any, [number]) :: {:reply, number, [number]}
  def handle_call({:sub, a, b}, _from, state) do
    ans = a - b
    {:reply, ans, [ans | state]}
  end

  @impl true
  @spec handle_call({:mult, number, number}, any, [number]) :: {:reply, number, [number]}
  def handle_call({:mult, a, b}, _from, state) do
    ans = a * b
    {:reply, ans, [ans | state]}
  end

  @impl true
  @spec handle_call({:div, number, number}, any, [number]) :: {:reply, number, [number]}
  def handle_call({:div, a, b}, _from, state) do

    if b == 0 do
      {:stop, "Illegal OP", "Cannot divide by ZERO", state}
    else
      ans = a / b
      {:reply, ans, [ans | state]}
    end
  end
end
