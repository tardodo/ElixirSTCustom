defmodule PasswordLock do
  use GenServer

  # -------------#
  # Client - API #
  # -------------#

  @moduledoc """
  Documentation for PasswordLock.
  locks the password
  """
  @doc """
  Initiate with the given password .
  """
  def start_link(password) do
    GenServer.start_link(__MODULE__, password, [])
  end

  @doc """
  Unlocks the given password
  """
  def unlock(server_pid, password) do
    GenServer.call(server_pid, {:unlock, password})
  end

  @doc """
  resets the given password
  """
  def reset(server_pid, {old_password,new_password}) do
    GenServer.call(server_pid, {:reset, {old_password,new_password}})
  end

  ##---------- ##
  #Server - API #
  ##-----------##

  def init(password) do
    {:ok, [password]} # ----------- state is stored as list of passwords
  end

  @spec handle_call({:unlock, binary}, any, [binary]) :: {:reply, atom, [binary]}
  def handle_call({:unlock, password}, _from, passwords) do # ----> aynchronous request
    if password in passwords do
      {:reply, :ok, passwords}

    else
     write_to_logfile password
    {:reply, {:error,"wrongpassword"}, passwords}
    end
  end

  @spec handle_call({:reset, {binary, binary}}, any, [binary]) :: {:reply, atom, [binary]}
  def handle_call({:reset, {old_password,new_password}}, _from, passwords) do
    if old_password in passwords do
      new_state = List.delete(passwords,old_password)
      {:reply, :ok, [new_password | new_state]}
    else
      write_to_logfile new_password
    {:reply, {:error,"wrongpassword"}, passwords}

    end

  end

    ### log the passed text to the logged file
    defp write_to_logfile text do
     {:ok,pid} = PasswordLogger.start_link()
     PasswordLogger.log_incorrect pid,"wrong_password #{text}"
    end

end
