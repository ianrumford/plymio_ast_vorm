defmodule PlymioAstVormDoctest1Test do

  use ExUnit.Case, async: true
  use PlymioAstVormHelperTest
  import Plymio.Ast.Vorm

  doctest Plymio.Ast.Vorm

end
