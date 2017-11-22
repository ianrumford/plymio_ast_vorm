defmodule Plymio.Ast.Vorm.Vormen.Transform do

  @moduledoc false

  alias Plymio.Option.Utility, as: POU
  alias Plymio.Ast.Utility, as: PAU
  alias Plymio.Ast.Vorm.Error, as: PAVE

  use Plymio.Ast.Vorm.Attribute

  defdelegate new_error(opts), to: PAVE, as: :new
  defdelegate new_error_result(opts), to: PAVE, as: :new_result

  defdelegate list_wrap_flat_just(value), to: POU
  defdelegate list_wrap_flat_just_uniq(value), to: POU

  def vormen_transform_reduce_all_forms_fun(forms) do
    forms
  end

  def vormen_transform_reduce_empty_forms_fun(_forms) do
    []
  end

  def vormen_transform_reduce_accumulator_forms_fun(forms) do
    {forms, []}
  end

  def vormen_transform_then_fetch({form,_index}, _new_forms, forms)
  when is_list(forms) do
    forms ++ [form]
  end

  def vormen_transform_else_fetch({_form,_index}, _new_forms, forms)
  when is_list(forms) do
    forms
  end

  def vormen_transform_then_filter({form,_index}, _new_forms, forms)
  when is_list(forms) do
    forms ++ [form]
  end

  def vormen_transform_else_filter(_form_index, _new_forms, forms)
  when is_list(forms) do
    forms
  end

  def vormen_transform_then_reject(_form_index, _new_forms, forms)
  when is_list(forms) do
    forms
  end

  def vormen_transform_else_reject({form,_index}, _new_forms, forms)
  when is_list(forms) do
    forms ++ [form]
  end

  def vormen_transform_then_insert({form,_index}, new_forms, forms)
  when is_list(new_forms) and is_list(forms) do
    forms ++ new_forms ++ [form]
  end

  def vormen_transform_else_insert({form,_index}, new_forms, forms)
  when is_list(new_forms) and is_list(forms) do
    forms ++ [form]
  end

  def vormen_transform_then_replace(_form_index, new_forms, forms)
  when is_list(new_forms) and is_list(forms) do
    forms ++ new_forms
  end

  def vormen_transform_else_replace({form,_index}, new_forms, forms)
  when is_list(new_forms) and is_list(forms) do
    forms ++ [form]
  end

  def vormen_transform_then_pipe_before({form,_index}, new_forms, forms)
  when is_list(new_forms) and is_list(forms) do

    with {:ok, new_form} <- PAU.forms_pipe([form | new_forms]) do
      {:ok, forms ++ List.wrap(new_form)}
    else
      {:error, _} = result -> result
    end

  end

  def vormen_transform_else_pipe_before({form,_index}, new_forms, forms)
  when is_list(new_forms) and is_list(forms) do
    forms ++ [form]
  end

  def vormen_transform_then_pipe_after({form,_index}, new_forms, forms)
  when is_list(new_forms) and is_list(forms) do

    with {:ok, new_form} <- PAU.forms_pipe(new_forms ++ List.wrap(form)) do
      {:ok, forms ++ List.wrap(new_form)}
    else
      {:error, _} = result -> result
    end

  end

  def vormen_transform_else_pipe_after({form,_index}, new_forms, forms)
  when is_list(new_forms) and is_list(forms) do
    forms ++ [form]
  end

  def normalise_vormen_transform(transform)

  def normalise_vormen_transform(transform)
  when is_function(transform,1) do
    {:ok, transform}
  end

  def normalise_vormen_transform(transforms) when is_list(transforms) do

    transforms
    |> list_wrap_flat_just
    |> Enum.reduce_while([],
    fn fun, funs ->
      case fun |> is_function(1) do
        true -> {:cont, [fun | funs]}
        _ -> {:halt, new_error_result(m: "forms transform invalid", v: fun)}
      end
    end)
    |> case do
         {:error, _} = result -> result
         transforms ->

           case transforms do
             [fun] -> {:ok, fun}

             funs ->

               funs = funs |> Enum.reverse

               fun = fn forms ->

                 funs
                 |> Enum.reduce_while(forms,
                 fn f, forms ->

                   forms
                   |> f.()
                   |> case do
                        {:error, _} = result -> {:halt, result}
                        {:ok, forms} -> {:cont, forms}
                        forms -> {:cont, forms}
                      end

                 end)
                 |> case do
                      {:error, _} = result -> result
                      forms -> {:ok, forms}
                    end

               end

               {:ok, fun}

           end

       end

  end

  def normalise_vormen_transform(transform) do
    new_error_result(m: "forms transform invalid", v: transform)
  end

  def normalise_form_index_transform(transform)

  def normalise_form_index_transform(transform)
  when is_function(transform,1) do
    {:ok, transform}
  end

  def normalise_form_index_transform(transforms) when is_list(transforms) do

    transforms
    |> list_wrap_flat_just
    |> Enum.reduce_while([],
    fn fun, funs ->
      case fun |> is_function(1) do
        true -> {:cont, [fun | funs]}
        _ -> {:halt, new_error_result(m: "{form,index} transform invalid", v: fun)}
      end
    end)
    |> case do
         {:error, _} = result -> result
         transforms ->

           case transforms do
             [fun] -> {:ok, fun}

             funs ->

               funs = funs |> Enum.reverse

               fun = fn {form,index} ->

                 funs
                 |> Enum.reduce_while({form,index},
                 fn f, {form,index} ->

                   {form,index}
                   |> f.()
                   |> case do
                        {:error, _} = result -> {:halt, result}
                        {:ok, form} -> {:cont, {form,index}}
                        form -> {:cont, {form,index}}
                      end

                 end)
                 |> case do
                      {:error, _} = result -> result
                      {form,_index} -> {:ok, form}
                    end

               end

               {:ok, fun}

           end

       end

  end

  def normalise_form_index_transform(transform) do
    new_error_result(m: "{form,index} transform invalid", v: transform)
  end

  def normalise_form_walk_transform(transform)

  def normalise_form_walk_transform(transform)
  when is_function(transform,1) do

    {:ok, transform}
  end

  def normalise_form_walk_transform(transforms) when is_list(transforms) do

    transforms
    |> list_wrap_flat_just
    |> Enum.reduce_while([],
    fn fun, funs ->
      case fun |> is_function(1) do
        true -> {:cont, [fun | funs]}
        _ -> {:halt, new_error_result(m: "invalid form walk transform", v: fun)}
      end
    end)
    |> case do
         {:error, _} = result -> result
         transforms ->

           case transforms do
             [fun] -> {:ok, fun}

             funs ->

               funs = funs |> Enum.reverse

               fun = fn form ->
                 funs |> Enum.reduce(form, fn f, form -> f.(form) end)
               end

               {:ok, fun}

           end

       end

  end

  def normalise_form_walk_function(transform) do
    new_error_result(m: "invalid form walk transform", v: transform)
  end

  def normalise_form_postwalk_transform(transform)

  def normalise_form_postwalk_transform(transform) do

    with {:ok, fun_walk} <- transform |> normalise_form_walk_transform do

      fun_postwalk = fn {form,_index} ->
        {:ok, form |> Macro.postwalk(fun_walk)}
      end

      {:ok, fun_postwalk}

    else
      {:error, _} = result -> result
    end

  end

  def normalise_form_prewalk_transform(transform)

  def normalise_form_prewalk_transform(transform) do

    with {:ok, fun_walk} <- transform |> normalise_form_walk_transform do

      fun_prewalk = fn {form,_index} ->
        {:ok, form |> Macro.prewalk(fun_walk)}
      end

      {:ok, fun_prewalk}

    else
      {:error, _} = result -> result
    end

  end

  def normalise_form_traverse_transform(acc, pre, post)

  def normalise_form_traverse_transform(acc, pre, post) do

    [pre, post]
    |> Enum.reduce_while([],
    fn fun, funs ->
      case fun |> is_function(2) do
        true -> {:cont, [fun | funs]}
        _ -> {:halt, new_error_result(m: "form traverse function invalid", v: fun)}
      end
    end)
    |> case do
         {:error, _} = result -> result
         [fun_pre, fun_post] ->

           fun = fn {form,_index} ->

             with {form, _acc} <- form |> Macro.traverse(acc, fun_pre, fun_post) do
               {:ok, form}
             else
               x -> new_error_result(m: "form traverse failed", v: x)
             end

           end

           {:ok, fun}

       end

  end

  def normalise_form_traverse_transform(transform) do
    new_error_result(m: "transform not (any -> any)", v: transform)
  end

  def normalise_traverse_function(transform)

  def normalise_traverse_function(transform)
  when is_function(transform,2) do
    {:ok, transform}
  end

  def normalise_traverse_function(transform) do
    new_error_result(m: "transform not (any -> any)", v: transform)
  end

end
