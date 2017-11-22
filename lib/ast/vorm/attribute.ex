defmodule Plymio.Ast.Vorm.Attribute do

  @moduledoc false

  defmacro __using__(_opts \\ []) do

    quote do

      @pav_value_forms_initial_value []
      @pav_value_vorm_initial_value nil

      @pav_key_struct_id :__struct__

      @pav_key_forms :forms
      @pav_key_vorm :vorm

      @pav_key_message :message
      @pav_key_value :value
      @pav_key_error :error

      @pav_key_filter :filter
      @pav_key_reject :reject
      @pav_key_insert :insert
      @pav_key_replace :replace
      @pav_key_reduce :reduce
      @pav_key_transform :transform
      @pav_key_postwalk :postwalk
      @pav_key_prewalk :prewalk
      @pav_key_traverse :traverse
      @pav_key_add :add
      @pav_key_fetch :fetch
      @pav_key_put :put
      @pav_key_pipe_before :pipe_before
      @pav_key_pipe_after :pipe_after

      @pav_key_append :append

      @pav_key_fun_pred :fun_pred
      @pav_key_fun_then :fun_then
      @pav_key_fun_else :fun_else

      @pav_key_fun_initial_value :fun_initial_value

      @pav_keys_pred_then_else [
        @pav_key_fun_pred,
        @pav_key_fun_then,
        @pav_key_fun_else,
      ]

      @pav_keys_then_else [
        @pav_key_fun_then,
        @pav_key_fun_else,
      ]

      @pav_keys_pred [
        @pav_key_fun_pred,
      ]

      # input to reduce
      @pav_key_source_forms :source_forms
      # initial value for reduce
      @pav_key_initial_forms :initial_forms
      @pav_key_new_forms :new_forms

      @pav_alias_forms {@pav_key_forms, [:form]}

      @pav_transform_type_filter @pav_key_filter
      @pav_transform_type_reject @pav_key_reject
      @pav_transform_type_insert @pav_key_insert
      @pav_transform_type_replace @pav_key_replace
      @pav_transform_type_reduce @pav_key_reduce
      @pav_transform_type_transform @pav_key_transform
      @pav_transform_type_pipe_before @pav_key_pipe_before
      @pav_transform_type_pipe_after @pav_key_pipe_after
      @pav_transform_type_fetch @pav_key_fetch

      @pav_transform_types [
        @pav_transform_type_filter,
        @pav_transform_type_reject,
        @pav_transform_type_insert,
        @pav_transform_type_replace,
        @pav_transform_type_reduce,
        @pav_transform_type_transform,
        @pav_transform_type_pipe_before,
        @pav_transform_type_pipe_after,
        @pav_transform_type_fetch,
      ]

      @pav_pipeline_verb_filter @pav_key_filter
      @pav_pipeline_verb_reject @pav_key_reject
      @pav_pipeline_verb_insert @pav_key_insert
      @pav_pipeline_verb_replace @pav_key_replace
      @pav_pipeline_verb_reduce @pav_key_reduce
      @pav_pipeline_verb_transform @pav_key_transform
      @pav_pipeline_verb_postwalk @pav_key_postwalk
      @pav_pipeline_verb_prewalk @pav_key_prewalk
      @pav_pipeline_verb_traverse @pav_key_traverse
      @pav_pipeline_verb_add @pav_key_add
      @pav_pipeline_verb_put @pav_key_put
      @pav_pipeline_verb_pipe_before @pav_key_pipe_before
      @pav_pipeline_verb_pipe_after @pav_key_pipe_after

      @pav_pipeline_verb_alias_filter {@pav_pipeline_verb_filter, [:filter_form, :filter_forms]}
      @pav_pipeline_verb_alias_reject {@pav_pipeline_verb_reject, [:reject_form, :reject_forms]}
      @pav_pipeline_verb_alias_insert {@pav_pipeline_verb_insert, [:insert_form, :insert_forms]}
      @pav_pipeline_verb_alias_replace {@pav_pipeline_verb_replace, [:replace_form, :replace_forms]}
      @pav_pipeline_verb_alias_reduce {@pav_pipeline_verb_reduce, [:reduce_form, :reduce_forms]}
      @pav_pipeline_verb_alias_transform {@pav_pipeline_verb_transform, [:transform_form, :transform_forms]}
      @pav_pipeline_verb_alias_postwalk {@pav_pipeline_verb_postwalk, [:postwalk_form, :postwalk_forms]}
      @pav_pipeline_verb_alias_prewalk {@pav_pipeline_verb_prewalk, [:prewalk_form, :prewalk_forms]}
      @pav_pipeline_verb_alias_traverse {@pav_pipeline_verb_traverse, [:traverse_form, :traverse_forms]}
      @pav_pipeline_verb_alias_add {@pav_pipeline_verb_add, [:add_form, :add_forms]}
      @pav_pipeline_verb_alias_put {@pav_pipeline_verb_put, [:put_form, :put_forms]}
      @pav_pipeline_verb_alias_pipe_before {@pav_pipeline_verb_pipe_before, [:pipe_before_form, :pipe_before_forms]}
      @pav_pipeline_verb_alias_pipe_after {@pav_pipeline_verb_pipe_after, [:pipe_after_form, :pipe_after_forms]}

      @pav_pipeline_verb_aliases [

        @pav_pipeline_verb_alias_filter,
        @pav_pipeline_verb_alias_reject,
        @pav_pipeline_verb_alias_insert,
        @pav_pipeline_verb_alias_replace,
        @pav_pipeline_verb_alias_reduce,
        @pav_pipeline_verb_alias_transform,
        @pav_pipeline_verb_alias_postwalk,
        @pav_pipeline_verb_alias_prewalk,
        @pav_pipeline_verb_alias_traverse,
        @pav_pipeline_verb_alias_add,
        @pav_pipeline_verb_alias_put,
        @pav_pipeline_verb_alias_pipe_before,
        @pav_pipeline_verb_alias_pipe_after,

      ]

    end

  end

end

