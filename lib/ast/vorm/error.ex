defmodule Plymio.Ast.Vorm.Error do

  @moduledoc false

  require Plymio.Option.Utility, as: POU
  use Plymio.Ast.Vorm.Attribute

  @pav_struct_kvs_aliases [

    {@pav_key_message, [:m, :msg]},
    {@pav_key_value, [:v]},
    {@pav_key_error, [:e]}

    ]

  @pav_struct_dict_aliases @pav_struct_kvs_aliases
  |> POU.opts_create_aliases_dict

  @pav_error_defstruct @pav_struct_kvs_aliases
  |> Enum.map(fn {k,_v} -> {k, nil} end)

  def opts_canon_keys!(opts, dict \\ @pav_struct_dict_aliases) do
    opts |> POU.opts_canon_keys!(dict)
  end

  defexception @pav_error_defstruct

  @type opts :: Keyword.t
  @type t :: %__MODULE__{}
  @type kv :: {any,any}
  @type error :: any

  @spec new(opts) :: {:ok, t} | {:error, error}

  def new(opts \\ [])

  def new([]) do
    {:ok, %__MODULE__{}}
  end

  def new(opts) do
    with {:ok, %__MODULE__{} = pase} <- new() do
      pase |> update(opts)
    else
      {:error, _} = result -> result
    end
  end

  def new!(opts \\ []) do

    opts
    |> new()
    |> case do
         {:ok, %__MODULE__{} = state} -> state
         {:error, error} -> raise error
       end

  end

  def new_result(opts \\ []) do

    opts
    |> new
    |> case do

         {:ok, %__MODULE__{} = pase} -> {:error, pase}

         {:error, error} ->

           case error |> Exception.exception? do

             true -> raise error

           end

       end

  end

  @spec update_field(t, kv) :: {:ok, t} | {:error, error}

  defp update_field(state, kv)

  defp update_field(%__MODULE__{} = state, {k,v})
  when k in [
    @pav_key_message,
  ] do

    cond do

      is_binary(v) -> state |> Map.put(k, v)
      is_atom(v) -> state |> Map.put(k, v |> to_string)

      true -> {:error, %ArgumentError{message: "expected valid #{inspect k}; got #{inspect v}"}}

    end

  end

  defp update_field(%__MODULE__{} = state, {k,v})
  when k in [
    @pav_key_value,
    @pav_key_error,
  ] do

    state |> struct!([{k, v}])

  end

  @spec update(t, opts) :: {:ok, t} | {:error, error}

  def update(state, opts \\ [])

  def update(%__MODULE__{} = state, []) do
    {:ok, state}
  end

  def update(%__MODULE__{} = state, opts) when is_list(opts) do

    opts
    |> POU.opts_canon_keys!(@pav_struct_dict_aliases)
    |> Enum.reduce_while(state, fn {k,v}, s ->

      s
      |> update_field({k,v})
      |> case do
           %__MODULE__{} = s -> {:cont, s}
           {:ok, %__MODULE__{} = s} -> {:cont, s}
           {:error, error} -> {:halt, error}
         end

    end)
    |> case do
         {:error, _} = result -> result
         value -> {:ok, value}
       end

  end

  def update!(%__MODULE__{} = state, opts \\ []) do

    state
    |> update(opts)
    |> case do
         {:ok, %__MODULE__{} = state} -> state
         {:error, error} -> raise error
       end

  end

  def message(%__MODULE__{} = pase) do

    pase
    |> Map.from_struct
    |> format_error_message

  end

  def format_error_message(opts \\ [])

  def format_error_message(opts) when is_map(opts) do
    opts |> Map.to_list |> format_error_message
  end

  def format_error_message(opts) when is_list(opts)do

    [
      message: nil,
      value: nil,
      error: nil,
    ]
    |> Enum.map(fn {k,_v} -> {k, opts |> Keyword.get(k)} end)
    |> Enum.reject(fn
      {_k, nil} -> true
      _ -> false
    end)
    |> Enum.reduce([], fn

    {@pav_key_message, message}, messages -> ["#{message}" | messages]

      {@pav_key_value, value}, messages -> ["got: #{inspect value}" | messages]

      {@pav_key_error, error}, messages ->

          message = cond do

            Exception.exception?(error) -> "reason: #{Exception.message(error)}"

            is_binary(error) -> error

            true -> "reason: #{inspect error}"

        end

        [message | messages]

    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.reverse
    |> Enum.join("; ")

  end

end
