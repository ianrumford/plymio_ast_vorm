ExUnit.start()

defmodule PlymioAstVormHelperTest do

  use Plymio.Ast.Vorm.Attribute

  defdelegate helper_vorm_show_forms(vorm), to: PlymioAstVormHelper
  defdelegate helper_vorm_test_forms!(vorm), to: PlymioAstVormHelper
  defdelegate helper_vorm_test_forms!(vorm, opts), to: PlymioAstVormHelper
  defdelegate helper_vorm_produce_pipeline!(vorm), to: PlymioAstVormHelper
  defdelegate helper_vorm_produce_pipeline!(vorm, opts), to: PlymioAstVormHelper

  def helper_forms_reduce(forms) do

    forms
    |> POU.list_wrap_flat_just
    |> case do

         [] -> nil

         forms ->

           quote do
             unquote_splicing(forms)
           end

       end

  end

  def helper_forms_reduce_text(forms) do

    forms
    |> helper_forms_reduce
    |> Macro.to_string

  end

  defmacro __using__(_opts \\ []) do

    quote do
      use ExUnit.Case, async: false
      use Plymio.Ast.Vorm.Attribute
      import PlymioAstVormHelperTest
      import PlymioAstEvalHelper
    end

  end

end

