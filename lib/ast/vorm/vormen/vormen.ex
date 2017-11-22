defmodule Plymio.Ast.Vorm.Vormen do

  @moduledoc false

  alias Plymio.Option.Utility, as: POU
  ##alias Plymio.Ast.Utility, as: PAU
  alias Plymio.Ast.Vorm.Utility, as: PAVU
  alias Plymio.Ast.Vorm.Vormen.Transform, as: PAVMT
  import Plymio.Ast.Vorm.Utility, only: [
    new_error_result: 1,
  ]

  use Plymio.Ast.Vorm.Attribute

  @type error :: struct
  @type form :: Macro.t
  @type forms :: [form]
  @type index :: integer

  @doc false

  @spec vormen_normalise(any) :: {:ok, forms} | {:error, error}

  def vormen_normalise(forms \\ [])

  def vormen_normalise(forms) when is_list(forms) do
    {:ok, forms}
  end

  def vormen_normalise(forms) do
    {:ok, [forms]}
  end

  @doc false

  @spec vormen_validate(any) :: {:ok, forms} | {:error, error}

  def vormen_validate(forms \\ [])

  def vormen_validate(forms) do
    with {:ok, forms} <- forms |> vormen_normalise,
         {:ok, _forms} = result <- forms |> PAVU.forms_validate do
      result
    else
      {:error, _} -> new_error_result(m: "expected valid forms", v: forms)
    end
  end

  @doc false

  def vormen_index_normalise(forms, index)

  def vormen_index_normalise(forms, nil) when is_list(forms) do
    {:ok, nil}
  end

  def vormen_index_normalise(forms, index)
  when is_list(forms) and is_integer(index) and index >= 0 do
    {:ok, index}
  end

  def vormen_index_normalise(forms, index)
  when is_list(forms) and is_integer(index) and index < 0 do
    {:ok, length(forms) + index}
  end

  def vormen_index_normalise(forms, _index) when not is_list(forms) do
    new_error_result(m: "forms invalid", v: forms)
  end

  def vormen_index_normalise(_forms, index) when not is_integer(index) do
    new_error_result(m: "index invalid", v: index)
  end

  @spec vormen_index_validate(any, any) :: {:ok, index} | {:error, error}

  defp vormen_index_validate(forms, index)

  defp vormen_index_validate(forms, index)
  when is_list(forms) and is_integer(index) do

    with {:ok, index} <- forms |> vormen_index_normalise(index) do

      index_max = length(forms) - 1

      case index do
        x when x >= 0 -> x
        x -> index_max + x + 1
      end
      |> fn
        ndx when ndx < 0 -> new_error_result(m: "index too small", v: ndx)
        ndx when ndx > index_max -> new_error_result(m: "index too large", v: ndx)
        ndx -> {:ok, ndx}
      end.()

    else
      {:error, _} = result -> result
    end

  end

  defp vormen_index_validate(_forms, index) when not is_integer(index) do
    new_error_result(m: "index invalid", v: index)
  end

  defp vormen_indices_validate(forms, indices) when is_list(forms) do

    indices
    |> List.wrap
    |> Enum.reduce_while([],
    fn index, indices ->

      case forms |> vormen_index_validate(index) do
        {:ok, index} -> {:cont, [index | indices]}
        {:error, _} = result -> {:halt, result}
      end

    end)
    |> case do
         {:error, _} = result -> result
         indices -> {:ok, indices |> Enum.reverse}
       end

  end

  defp vormen_indices(forms) when is_list(forms) do
    forms |> Enum.with_index |> Enum.map(&(elem(&1,1)))
  end

  defp vormen_indices_normalise(forms, indices)

  defp vormen_indices_normalise(forms, nil) when is_list(forms) do
    {:ok, forms |> vormen_indices}
  end

  defp vormen_indices_normalise(forms, indices) when is_list(forms) do

    indices = cond do
      is_integer(indices) -> indices

      is_atom(indices) ->

        case indices do
          :first -> 0
          :last -> -1
          :all -> forms |> vormen_indices
          x -> x
        end

      Keyword.keyword?(indices) -> indices |> Keyword.values
      is_list(indices) -> indices
      is_map(indices) -> indices |> Map.keys
      true -> indices
    end
    |> POU.list_wrap_flat_just_uniq

    forms |> vormen_indices_validate(indices)

  end

  defp vormen_transform(forms, opts)

  defp vormen_transform(forms, []) when is_list(forms) do
    {:ok, forms}
  end

  defp vormen_transform(forms, opts) when is_list(forms) and is_list(opts) do

    with {:ok, reduce_while_transform} <- opts |> vormen_build_reduce_while_transform do

      forms
      |> reduce_while_transform.()

    else
      {:error, _} = result -> result
    end

  end

  @vorm_transform_types_opts %{

    filter: [
      {@pav_key_fun_then, &PAVMT.vormen_transform_then_filter/3},
      {@pav_key_fun_else, &PAVMT.vormen_transform_else_filter/3},
      {@pav_key_fun_initial_value, &PAVMT.vormen_transform_reduce_empty_forms_fun/1},
    ],

    reject: [
      {@pav_key_fun_then, &PAVMT.vormen_transform_then_reject/3},
      {@pav_key_fun_else, &PAVMT.vormen_transform_else_reject/3},
      {@pav_key_fun_initial_value, &PAVMT.vormen_transform_reduce_empty_forms_fun/1},
    ],

    insert: [
      {@pav_key_fun_then, &PAVMT.vormen_transform_then_insert/3},
      {@pav_key_fun_else, &PAVMT.vormen_transform_else_insert/3},
      {@pav_key_fun_initial_value, &PAVMT.vormen_transform_reduce_empty_forms_fun/1},
    ],

    replace: [
      {@pav_key_fun_then, &PAVMT.vormen_transform_then_replace/3},
      {@pav_key_fun_else, &PAVMT.vormen_transform_else_replace/3},
      {@pav_key_fun_initial_value, &PAVMT.vormen_transform_reduce_empty_forms_fun/1},
    ],

    transform: [
      {@pav_key_fun_initial_value, &PAVMT.vormen_transform_reduce_empty_forms_fun/1},
    ],

    pipe_before: [
      {@pav_key_fun_then, &PAVMT.vormen_transform_then_pipe_before/3},
      {@pav_key_fun_else, &PAVMT.vormen_transform_else_pipe_before/3},
      {@pav_key_fun_initial_value, &PAVMT.vormen_transform_reduce_empty_forms_fun/1},
    ],

    pipe_after: [
      {@pav_key_fun_then, &PAVMT.vormen_transform_then_pipe_after/3},
      {@pav_key_fun_else, &PAVMT.vormen_transform_else_pipe_after/3},
      {@pav_key_fun_initial_value, &PAVMT.vormen_transform_reduce_empty_forms_fun/1},
    ],

    fetch: [
      {@pav_key_fun_then, &PAVMT.vormen_transform_then_fetch/3},
      {@pav_key_fun_else, &PAVMT.vormen_transform_else_fetch/3},
      {@pav_key_fun_initial_value, &PAVMT.vormen_transform_reduce_empty_forms_fun/1},
    ],

  }

  def vormen_create_transform(forms, form_range, transform_type, transform_opts \\ [])

  def vormen_create_transform(forms, form_range, transform_type, transform_opts) do

    with {:ok, type_opts} <- @vorm_transform_types_opts |> Map.fetch(transform_type) do

      with true <- transform_opts |> Keyword.keyword? do

        with {:ok, form_predicate} <- forms |> form_range_create_predicate(form_range) do

          # order is important
          all_opts =  type_opts ++ transform_opts ++ [{@pav_key_fun_pred, form_predicate}]
          |> Keyword.new

          forms |> vormen_transform(all_opts)

        else
          {:error, _} = result -> result
        end

      else
        false -> new_error_result(m: "transform opts invalid", v: transform_opts)
      end

    else
      :error -> new_error_result(m: "transform type unknown", v: transform_type)
    end

  end

  defp vormen_build_reduce_while_transform(opts)

  defp vormen_build_reduce_while_transform(opts) when is_list(opts) do

    with {:ok, reduce_initial_value_fun} <- opts |> vormen_build_reduce_while_initial_value_fun,
         {:ok, reduce_while_fun} <- opts |> vormen_build_reduce_while_fun do

      reduce_while_transform = fn forms ->

        forms
        |> Stream.with_index
        |> Enum.reduce_while(reduce_initial_value_fun.(forms), reduce_while_fun)
        |> case do
             {:error, _} = result -> result
             forms -> {:ok, forms}
           end

      end

      {:ok, reduce_while_transform}

    else
      {:error, _} = result -> result
    end

  end

  defp vormen_build_reduce_while_initial_value_fun(opts)

  defp vormen_build_reduce_while_initial_value_fun(opts) when is_list(opts) do

    with {:ok, funs_opts} <- opts |> POU.opts_filter_keys([@pav_key_fun_initial_value]),
         {:ok, funs_opts} <- funs_opts |> POU.opts_predicate(fn {_k,v} -> is_function(v,1) end) do

      fun_reduce_initial_value = funs_opts
      |> Keyword.fetch!(@pav_key_fun_initial_value)

      {:ok,fun_reduce_initial_value}

    else
      {:error, _} = result -> result
    end

  end

  defp vormen_build_reduce_while_fun(opts)

  defp vormen_build_reduce_while_fun(opts) when is_list(opts) do

    with {:ok, pred_the_else_fun} <- opts |> vormen_build_pred_then_else_fun do

      fun = fn {_form, _index} = fi, forms ->

        fi
        |> pred_the_else_fun.(forms)
        |> case do
             {:error, _} = result -> {:halt, result}
             {:ok, forms} -> {:cont, forms}
             forms -> {:cont, forms}
           end

      end

      {:ok, fun}
    else
      {:error, _} = result -> result
    end

  end

  defp vormen_build_pred_then_else_fun(opts)

  defp vormen_build_pred_then_else_fun(opts) when is_list(opts) do

    with {:ok, opts} <- opts |> POU.opts_validate,
         {:ok, _} <- opts |> POU.opts_avoir_keys(@pav_keys_pred_then_else),
         {:ok, pred_opts} <- opts |> POU.opts_filter_keys(@pav_keys_pred),
         {:ok, _opts} <- pred_opts |> POU.opts_predicate(fn {_k,v} -> is_function(v,1) end),
         {:ok, then_else_opts} <- opts |> POU.opts_filter_keys(@pav_keys_then_else),
         {:ok, _opts} <- then_else_opts |> POU.opts_predicate(fn {_k,v} -> is_function(v,3) end) do

      # could be nil
      new_forms = opts |> Keyword.get(@pav_key_new_forms)

      fun_pred = opts |> Keyword.fetch!(@pav_key_fun_pred)

      fun_then = opts |> Keyword.fetch!(@pav_key_fun_then)

      fun_else = opts |> Keyword.fetch!(@pav_key_fun_else)

      fun = fn {_form, _index} = fi, forms ->

        fi
        |> fun_pred.()
        |> case do
             true -> fun_then.(fi, new_forms, forms)
             _ -> fun_else.(fi, new_forms, forms)
           end
        |> case do
             {:ok, _} = result -> result
             {:error, _} = result -> result
             forms -> {:ok, forms}
           end

      end

      {:ok, fun}

    else
      {:error, _} = result -> result
    end

  end

  def form_range_create_predicate(forms, range)

  # range == nil => all forms
  def form_range_create_predicate(forms, nil) when is_list(forms) do
    {:ok, fn _ -> true end}
  end

  # range = arity 1 fun
  def form_range_create_predicate(forms, range)
  when is_list(forms) and is_function(range, 1) do
    {:ok, range}
  end

  def form_range_create_predicate(forms, range) when is_list(forms) do

    with {:ok, indices} <- forms |> vormen_indices_normalise(range) do

      range_map = indices |> Map.new(fn k -> {k, nil} end)

      fun = fn

        {_form,index} ->

        range_map |> Map.has_key?(index)

        x ->

        raise ArgumentError, message: "predicated expected {form,index}; got #{inspect x}"

      end

      {:ok, fun}

    else
      {:error, _} = result -> result
    end

  end

  def form_range_create_predicate(forms, index) do
    new_error_result(m: "forms or index invalid", e: "forms #{inspect forms}; index #{inspect index}")
  end

end

