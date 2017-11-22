defmodule PlymioAstVormReify1ModuleTest do

  use PlymioAstVormHelperTest
  import Plymio.Ast.Vorm, only: [reify_pipeline: 1]

  [

    add: quote do
           def add_xy(x, y) do
             x + y
           end
         end,

    add: quote do
           def sub_xy(x, y) do
             x - y
           end
    end,

    add: quote do
           def mul_xy(x, y) do
             x * y
           end
         end,

  ]
  |> reify_pipeline

end

defmodule PlymioAstVormReify1Test do

  use PlymioAstVormHelperTest
  import PlymioAstVormReify1ModuleTest

  test "add_xy: 100a" do

    assert 42 = add_xy(41,1)
    assert 42 = add_xy(3,39)
    assert 42 = add_xy(-5,47)

  end

  test "sub_xy: 100a" do

    assert -42 = sub_xy(1,43)
    assert 42 = sub_xy(-3,-45)
    assert 42 = sub_xy(-5,-47)

  end

  test "mul_xy: 100a" do

    assert 2 = mul_xy(1,2)
    assert 30 = mul_xy(6,5)
    assert 15 = mul_xy(-5,-3)

  end

end
