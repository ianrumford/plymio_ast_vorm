defmodule PlymioAstVormHelper do

  alias Plymio.Ast.Vorm, as: PAV
  use Plymio.Ast.Vorm.Attribute
  use PlymioAstEvalAttributeHelper

  import PlymioAstEvalHelper, only: [
    helper_ast_eval: 2,
    helper_ast_eval_normalise_error: 1,
    helper_ast_eval_compare: 3,
    helper_ast_eval_opts_canon_keys!: 1,
    ]

  @helper_vorm_produce_opts_keys_eval [
    @helper_opts_key_binding,
    @helper_opts_key_expect_value,
    @helper_opts_key_expect_text,
    @helper_opts_key_expect_texts,
    @helper_opts_key_expect_form,

  ]

  def helper_vorm_produce(vorm, opts)

  def helper_vorm_produce(%PAV{}= vorm, opts) when is_list(opts) do

    opts = opts |> helper_ast_eval_opts_canon_keys!

    eval_opts = opts |> Keyword.take(@helper_vorm_produce_opts_keys_eval)

    with {:ok, {forms, %PAV{}}} <- vorm |> PAV.produce do
      forms |> helper_ast_eval(eval_opts)
    else
      {:error, _} = result -> result
    end

  end

  def helper_vorm_produce_pipeline(pipeline, opts \\ [])

  def helper_vorm_produce_pipeline(pipeline, opts) when is_list(opts) do

    opts = opts |> helper_ast_eval_opts_canon_keys!

    eval_opts = opts |> Keyword.take(@helper_vorm_produce_opts_keys_eval)

    with {:ok, vorm} <- PAV.new(),
         {:ok, {forms, %PAV{}}} <- pipeline |> PAV.produce_pipeline(vorm) do
      forms |> helper_ast_eval(eval_opts)
    else
      {:error, _} = result -> result
    end

  end

  def helper_vorm_produce_pipeline!(pipeline, opts \\ [])

  def helper_vorm_produce_pipeline!(pipeline, opts) when is_list(opts) do

    opts = opts |> helper_ast_eval_opts_canon_keys!

    pipeline
    |> helper_vorm_produce_pipeline(opts)
    |> case do

         {:ok, {_result, _text, _ast} = result} -> result

         {:error, error} = result->

             case opts |> Keyword.has_key?(@helper_opts_key_expect_error) do

               true ->

                 error
                 |> helper_ast_eval_normalise_error
                 |> helper_ast_eval_compare(@helper_opts_key_expect_error, opts)
                 |> case do
                      # expected error matches
                      {:ok, actual_error} -> {:ok, {actual_error, nil, nil}}
                      _ -> result
                    end

               # no error to compare with
               _ -> result

             end
             |> case do
                  # expected error matches
                  {:ok, _} = result -> result

                  {:error, error} ->

                    case error |> Exception.exception? do
                      true -> raise error
                    end

                end

       end

  end

  def helper_vorm_show_forms(vorm, opts \\ [])

  def helper_vorm_show_forms(%PAV{} = vorm, _opts) do

    with {:ok, {forms, %PAV{}}} <- vorm |> PAV.express do
      {:ok, forms |> Enum.map(&Macro.to_string/1)}
    else
      {:error, _} = result -> result
    end

  end

  def helper_vorm_show_forms!(vorm, opts \\ []) do

    with {:ok, forms} <- vorm |> helper_vorm_show_forms(opts) do
      forms
    else
      {:error, error} -> raise error
    end

  end

  def helper_vorm_test_forms!(vorm, opts \\ [])

  def helper_vorm_test_forms!(%PAV{} = vorm, opts) do

    vorm
    |> helper_vorm_produce(opts)
    |> case do

         {:ok, {result, texts, _forms}} -> {result, texts}

         {:error, error} -> raise error

       end

  end

end
