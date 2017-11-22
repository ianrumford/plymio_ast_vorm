defmodule PlymioAstTransformBasic1Test do

  use PlymioAstVormHelperTest

  test "replace: 100a" do

    [
      quote(do: x = x + x),
      quote(do: x = x * x),
      quote(do: x = rem(x, 13)),

      # replace the last form
      {:replace, -1, quote(do: x = x - 1)},

    ]
    |> helper_vorm_produce_pipeline!(binding: [x: 42], result: 7055)

    [
      quote(do: x = x + x),
      quote(do: x = x * x),
      quote(do: x = rem(x, 13)),

      # replace the first form with two forms
      {:replace, 0, [quote(do: x = x - 1), quote(do: x = x - y)]},

      ]
      |> helper_vorm_produce_pipeline!(binding: [x: 42, y: 9], result: 10)

  end

  test "insert_forms: 100a" do

    [
      quote(do: x = x + x),
      quote(do: x = x * x),
      quote(do: x = rem(x, 13)),

      # insert before the first form
      {:insert_forms, 1, quote(do: x = x - 1)},

    ]
    |> helper_vorm_produce_pipeline!(binding: [x: 42], result: 12)

    [
      quote(do: x = x + x),
      quote(do: x = x * x),
      quote(do: x = rem(x, 13)),

      # insert before the last form
      {:insert_form, -1, quote(do: x = x - 1)},

    ]
    |> helper_vorm_produce_pipeline!(binding: [x: 42], result: 9)

    [
      quote(do: x = x + x),
      quote(do: x = x * x),
      quote(do: x = rem(x, 13)),

      # insert the first form with two forms
      {:insert, 0, [quote(do: x = x - 1), quote(do: x = x - y)]},

    ]
    |> helper_vorm_produce_pipeline!(binding: [x: 42, y: 9], result: 1)

    [
      quote(do: x = x + x),
      quote(do: x = x * x),
      quote(do: x = rem(x, 13)),

      # insert the first form with two forms
      {:insert_forms, :append, [quote(do: x = x - 1), quote(do: x = x - y)]},

    ]
    |> helper_vorm_produce_pipeline!(binding: [x: 42, y: 9], result: 0)

    [

      {:insert_forms, nil, [quote(do: x = x - 1), quote(do: x = x - y)]},

    ]
    |> helper_vorm_produce_pipeline!(binding: [x: 52, y: 9], result: 42)

  end

end
