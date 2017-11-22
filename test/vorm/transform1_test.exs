defmodule PlymioAstTransformTransform1Test do

  use PlymioAstVormHelperTest

  test "postwalk: single ast w/form_range 200a" do

    [
      Macro.var(:x, nil),

      {:postwalk, 0,
       fn
         {:x, [], nil} -> Macro.var(:y, nil)
         # passthru
         any -> any
       end},

    ]
    |> helper_vorm_produce_pipeline!(
      binding: [y: 99], compare_text: "y", compare_form: Macro.var(:y, nil), result: 99)

  end

  test "postwalk: single ast invalid range 300a" do

    [
      Macro.var(:x, nil),

      {:postwalk, 999,
       fn
         {:x, [], nil} -> Macro.var(:y, nil)
         # passthru
         any -> any
       end},

    ]
    |> helper_vorm_produce_pipeline!(
      error: "index too large; got: 999")

    [
      Macro.var(:x, nil),

      {:postwalk, :this_range_is_invalid,
       fn
         {:x, [], nil} -> Macro.var(:y, nil)
         # passthru
         any -> any
       end},

    ]
    |> helper_vorm_produce_pipeline!(
      error: "index invalid; got: :this_range_is_invalid")

  end

  test "transform: single ast 100a" do

    [
      quote do
      x = x + x
      x = x * x
      x = rem(x, 13)
    end,

      {:transform, 0,
       fn ast ->
         quote do
           unquote(ast)
           y = x + 1
         end
       end},

    ]
    |> helper_vorm_produce_pipeline!(binding: [x: 42], result: 11)

  end

end
