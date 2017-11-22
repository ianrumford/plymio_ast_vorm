defmodule Plymio.Ast.Vorm.Utility.Pipeline do

  @moduledoc false

  alias Plymio.Option.Utility, as: POU
  alias Plymio.Ast.Vorm, as: PAV
  import Plymio.Ast.Vorm.Utility, only: [
    new_error_result: 1,
  ]

  use Plymio.Ast.Vorm.Attribute

  @type items :: list
  @type t :: %PAV{}
  @type error :: struct

  @pav_pipeline_verbs_dict_aliases @pav_pipeline_verb_aliases
  |> POU.opts_create_aliases_dict

  def opts_canon_keys(opts, dict \\ @pav_pipeline_verbs_dict_aliases) do
    opts |> POU.opts_canonical_keys(dict)
  end

  defp canonical_verb(verb, dict \\ @pav_pipeline_verbs_dict_aliases) do
    verb |> POU.canonical_key(dict)
  end

  defp pipeline_normalise_item_verb(verb)

  defp pipeline_normalise_item_verb(verb) do
    verb |> canonical_verb
  end

  defp pipeline_normalise_item_args(kv)

  defp pipeline_normalise_item_args({verb,args})
  when verb in [:add] do
    {:ok, {verb, [args |> List.wrap]}}
  end

  defp pipeline_normalise_item_args({verb,args})
  when verb in [:insert, :replace]
  and is_list(args)
  and length(args) >= 2 do
    # slurp up all the args after the first (form_range) into new_forms
    {:ok, {verb, [args |> hd, args |> tl |> List.wrap]}}
  end

  defp pipeline_normalise_item_args({verb,args}) when is_list(args) do
    {:ok, {verb, args}}
  end

  defp pipeline_normalise_item_args({verb,args}) when is_nil(args) do
    {:ok, {verb, [nil]}}
  end

  defp pipeline_normalise_item_args({verb,args}) do
    {:ok, {verb, [args]}}
  end

  defp pipeline_normalise_item(item)

  defp pipeline_normalise_item({k,v}) do

    with {:ok, verb} <- k |> pipeline_normalise_item_verb,
         {:ok, {_k,v}} = result when is_list(v) <- {verb, v} |> pipeline_normalise_item_args do
      result
    else
      {:error, %KeyError{} = error} -> {:error, error |> struct!(term: {k,v})}
      {:error, _} = result -> result
    end

  end

  defp pipeline_normalise_item(v) when is_tuple(v) do

    {[verb], args} = v
    |> Tuple.to_list |> Enum.split(1)

    # if a known verb convert. otherwise must be an ast
    with {:ok, verb} <- verb |> canonical_verb do

      {verb, args} |> pipeline_normalise_item

    else

      _ ->

        v
        |> Macro.validate
        |> case do
             :ok -> {:ok, {@pav_pipeline_verb_add, [v]}}
             _ -> new_error_result(m: "form invalid", v: v)
           end

    end

  end

  defp pipeline_normalise_item(v) when is_list(v) do

    case v |> Keyword.keyword? do

      true ->

        v
        |> pipeline_normalise_items

      _ ->

        v
        |> Enum.all?(fn v -> :ok = Macro.validate(v) end)
        |> case do
             true -> {:ok, {@pav_pipeline_verb_add, [v]}}
             _ -> new_error_result(m: "invalid item", v: v)
           end
    end

  end

  defp pipeline_normalise_items(items)

  defp pipeline_normalise_items(items) when is_list(items) do

    items
    |> Enum.reduce_while([], fn v, tuples ->

      v
      |> pipeline_normalise_item
      |> case do
           {:error, _} = result -> {:halt, result}
           {:ok, values} -> {:cont, [values | tuples]}
           {k,v} -> {:cont, [{k,v} | tuples]}
         end

    end)
    |> case do
         {:error, _} = result -> result
         items -> {:ok, items |> Enum.reverse |> List.flatten}
       end

  end

  defp pipeline_apply_items(state, items)

  defp pipeline_apply_items(%PAV{} = state, items) when is_list(items) do

    items
    |> Enum.reduce_while(state,
    fn {verb,args}, vorm ->

      apply(PAV, verb, [vorm | args])
      |> case do
           {:error, _} = result -> {:halt, result}
           {:ok, %PAV{} = vorm} -> {:cont, vorm}
         end

    end)
    |> case do
         {:error, _} = result -> result
         %PAV{} = vorm -> {:ok, vorm}
       end

  end

  @doc false

  @spec pipeline(t, items) :: {:ok, t} | {:error, error}

  def pipeline(vorm, items \\ [])

  def pipeline(%PAV{} = state, []) do
    {:ok, state}
  end

  def pipeline(%PAV{} = state, items) when is_list(items) do

    with {:ok, tuples} <- items |> pipeline_normalise_items,
         {:ok, %PAV{}} = result <- state |> pipeline_apply_items(tuples) do
      result
    else
      {:error, _} = result -> result
    end

  end

end
