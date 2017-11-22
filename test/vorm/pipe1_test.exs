defmodule PlymioAstTransformPipe1Test do

  use PlymioAstVormHelperTest

  test "pipe_before: singleton ast 100a" do

    [
      Macro.var(:x, nil),
      pipe_before: [0, quote(do: fn v -> v + 1 end.())],
    ]
    |> helper_vorm_produce_pipeline!(result: 43, binding: [x: 42],
    texts: ["(fn v -> v + 1 end).(x)"])

  end

  test "pipe_before: singleton ast 200a" do

    [
      quote(do: x = x + 1),
      quote(do: x = x * x),
      pipe_before: [1, quote(do: fn v -> v + 1 end.())],
    ]
    |> helper_vorm_produce_pipeline!(result: 17, binding: [x: 3],
    texts: ["x = x + 1", "(fn v -> v + 1 end).(x = x * x)"])

  end

  test "pipe_before: singleton ast with index 100a" do

    [
      Macro.var(:x, nil),
      [pipe_before: [-1, {quote(do: Kernel.-(100)), 1}]]
    ]
    |> helper_vorm_produce_pipeline!(result: 58, binding: [x: 42, y: 99])

  end

  test "pipe_before: multiple asts 100a" do

    ast1 = quote(do: fn v -> v + 1 end.())

    ast2 = quote(do: fn v -> v * v end.())

    ast3 = quote(do: fn v -> 1 - v end.())

    [
      Macro.var(:x, nil),
      pipe_before: [0, [ast1, ast2, ast3]]
    ]
    |> helper_vorm_produce_pipeline!(result: -1848, binding: [x: 42])

  end

  test "pipe_before: multiple asts with indicies 100a" do

    [
      Macro.var(:x, nil),
      pipe_before: [0, [
        {quote(do: Kernel./(42)), 1},
        quote(do: List.wrap),
        {quote(do: Kernel.++([1,2,3])), 1},
        {quote(do: Enum.reduce([&Enum.sum/1, ], fn f, s -> f.(s) end)), 1}
      ]]
    ]
    |> helper_vorm_produce_pipeline!(result: 12.0, binding: [x: 7],
    texts: ["Enum.reduce([&Enum.sum/1], Kernel.++([1, 2, 3], List.wrap(Kernel./(42, x))), fn f, s -> f.(s) end)"])

  end

  test "pipe_after: singleton ast" do

    [
      quote(do: fn v -> v + 1 end.()),
      pipe_after: [0, Macro.var(:x, nil)]
    ]
    |> helper_vorm_produce_pipeline!(result: 43, binding: [x: 42],
    texts: "(fn v -> v + 1 end).(x)")

  end

  test "pipe_after: multiple asts" do

    [
      quote(do: fn v -> 1 - v end.()),
      pipe_after: [-1, [
        Macro.var(:x, nil),
        quote(do: fn v -> v + 1 end.()),
        quote(do: fn v -> v * v end.())
      ]]
    ]
    |> helper_vorm_produce_pipeline!(result: -1848, binding: [x: 42],
    texts: ["(fn v -> 1 - v end).((fn v -> v * v end).((fn v -> v + 1 end).(x)))"])

  end

end
