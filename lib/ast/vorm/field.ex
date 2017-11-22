defmodule Plymio.Ast.Vorm.Field do

  @moduledoc ~S"""
  Convenience wrappers for using a *vorm* held in a *struct*.

  Most of these wrappers assumes a *vorm* is, or will be, held in the `:vorm` field of a *struct* and allow function such as `add` to be called where the first argument is the struct.  For example `struct_vorm_add` looks like:

      def struct_vorm_add(struct, new_forms) do
        with {:ok, %{vorm: %Plymio.Ast.Vorm{} = vorm} = struct <- struct |> struct_vorm_ensure,
             {:ok, %Plymio.Ast.Vorm{} = vorm} <- vorm |> Plymio.Ast.Vorm.add(new_forms)  do
          {:ok, struct |> struct!(vorm: vorm)
        else
          {:error, _} = result -> result
        end
      end

  > Since most of the wrappers work in the same way, only `struct_vorm_add` will be documented below with illustrative examples.

  ## Documentation Terms

  In the documentation below these terms, usually in *italics*, are used to mean the same thing (e.g. *state*).

  ### *vorm*

  An instance of `Plymio.Ast.Vorm` (*%Plymio.Ast.Vorm{}*).

  ### *vorm field*

  The field called `:vorm` in a struct intended to hold a *vorm*.

  ### *state*

  A instance of a struct with a *vorm field*.

  """

  alias Plymio.Ast.Vorm, as: PAV
  alias Plymio.Ast.Vorm.Utility, as: PAVU
  import Plymio.Ast.Vorm.Utility, only: [
    new_error_result: 1,
  ]

  use Plymio.Ast.Vorm.Attribute

  @type t :: struct
  @type vorm :: %Plymio.Ast.Vorm{}
  @type error :: struct
  @type form :: Macro.t
  @type forms :: [form]

  @doc ~S"""
  `struct_vorm_ensure/1` take a *state* and checks if the *vorm field* already holds a *vorm*.

  If it has `{:ok, state}` is returned.

  If the *vorm field* is `nil`, a new, empty *vorm* is created, stored in the *vorm field*, and `{:ok, state}` returned.

  Any other value in the *vorm field* will cause an error.

  ## Examples

      iex> t = %PlymioAstVormFieldTest{}
      ...> match?(nil, t.vorm)
      true

      iex> {:ok, t} = %PlymioAstVormFieldTest{}
      ...> |> struct_vorm_ensure
      ...> match?(%Plymio.Ast.Vorm{}, t.vorm)
      true

      iex> {:ok, vorm} = Plymio.Ast.Vorm.new(form: quote(do: x = x + 1))
      ...> {:ok, t} = %PlymioAstVormFieldTest{vorm: vorm}
      ...> |> struct_vorm_ensure
      ...> t.vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {4, ["x = x + 1"]}

      iex> %PlymioAstVormFieldTest{vorm: :not_a_vorm}
      ...> |> struct_vorm_ensure
      {:error, %Plymio.Ast.Vorm.Error{error: nil, message: "vorm field invalid",
         value: %PlymioAstVormFieldTest{vorm: :not_a_vorm}}}

  """

  @spec struct_vorm_ensure(t) :: {:ok, t} | {:error, error}

  def struct_vorm_ensure(t)

  def struct_vorm_ensure(%{@pav_key_struct_id => _, @pav_key_vorm =>  %PAV{}} = state) do
    {:ok, state}
  end

  def struct_vorm_ensure(%{@pav_key_struct_id => _, @pav_key_vorm => form} = state)
  when @pav_value_vorm_initial_value == form do

    with {:ok, form_state} <- PAV.new() do
      {:ok, state |> struct!([{@pav_key_vorm, form_state}])}
    else
      {:error, _} = result -> result
    end
  end

  def struct_vorm_ensure(state) do
    new_error_result(m: "vorm field invalid", v: state)
  end

  @doc ~S"""
  `struct_vorm_update/2` take a *state* and a second argument.

  If the second argument is a *vorm*, the *state* is updated with it and `{:ok, state}` returned.

  Otherwise the second argument is passed to `new/1` and the new *vorm* stored in the *state*.

  ## Examples

      iex> {:ok, vorm} = Plymio.Ast.Vorm.new(form: quote(do: x = x + 1))
      ...> t = %PlymioAstVormFieldTest{}
      ...> {:ok, t} = t |> struct_vorm_update(vorm)
      ...> t.vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {4, ["x = x + 1"]}

      iex> t = %PlymioAstVormFieldTest{}
      ...> {:ok, t} = t |> struct_vorm_update(forms: quote(do: x = x + 1))
      ...> t.vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {4, ["x = x + 1"]}

      iex> t = %PlymioAstVormFieldTest{}
      ...> t |> struct_vorm_update(:not_new_opts)
      {:error, %Plymio.Ast.Vorm.Error{error: nil, message: "new vorm opts invalid", value: :not_new_opts}}

  """

  @spec struct_vorm_update(t, any) :: {:ok, t} | {:error, error}

  def struct_vorm_update(t, new_forms)

  def struct_vorm_update(%{@pav_key_struct_id => _} = state, %PAV{} = vorm) do

    {:ok, state |> struct!([{@pav_key_vorm, vorm}])}

  end

  def struct_vorm_update(%{@pav_key_struct_id => _} = state, opts) do

    with true <- opts |> Keyword.keyword? do

      with {:ok, %PAV{} = vorm} <- PAV.new(opts) do
        {:ok, state |> struct!([{@pav_key_vorm, vorm}])}
      else
        {:error, _} = result -> result
      end

    else
      false -> new_error_result(m: "new vorm opts invalid", v: opts)
    end

  end

  @doc ~S"""
  `struct_forms_update/2` take a *state* and a second argument.

  If the second argument is a *vorm*, the *state* is updated with the *vorm's* *forms* and `{:ok, state}` returned.

  Otherwise the second argument is assumed to be one or more forms and the *state* is updated with them, returning `{:ok, state}`.

  ## Examples

      iex> {:ok, vorm} = Plymio.Ast.Vorm.new(form: quote(do: x = x + 1))
      ...> t = %PlymioAstVormFieldTest{}
      ...> {:ok, t} = t |> struct_forms_update(vorm)
      ...> t.vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {4, ["x = x + 1"]}

      iex> t = %PlymioAstVormFieldTest{}
      ...> {:ok, t} = t |> struct_forms_update(forms: quote(do: x = x + 1))
      ...> t.vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {4, ["x = x + 1"]}

      iex> t = %PlymioAstVormFieldTest{}
      ...> {:ok, t} = t |> struct_forms_update(:a_valid_ast)
      ...> t.vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {:a_valid_ast, [":a_valid_ast"]}

  """

  @spec struct_forms_update(t, any) :: {:ok, t} | {:error, error}

  def struct_forms_update(t, new_forms)

  def struct_forms_update(%{@pav_key_struct_id => _} = state, %PAV{} = vorm) do

    state |> struct_vorm_update(vorm)

  end

  def struct_forms_update(%{@pav_key_struct_id => _} = state, forms) when is_list(forms) do

    with true <- forms |> Keyword.keyword? do
      state |> struct_vorm_update(forms)
    else
      false ->

        with {:ok, %{@pav_key_struct_id => _, @pav_key_vorm => %PAV{} = vorm} = state} <- state |> struct_vorm_ensure,
             {:ok, %PAV{} = vorm} <- vorm |> PAV.vorm_update_forms(forms) do
          {:ok, state |> struct!([{@pav_key_vorm, vorm}])}
        else
          {:error, _} = result -> result
        end

    end

  end

  def struct_forms_update(%{@pav_key_struct_id => _} = state, value) do

    with {:ok, _} <- value |> PAVU.form_validate do
      state |> struct_forms_update([value])
    else
      _ -> new_error_result(m: "new vorm forms invalid", v: value)
    end
  end

  @doc ~S"""
  `struct_vorm_reset/1` take a *state* and resets its *vorm field* to `nil` returning `{:ok, state}`.

  ## Examples

      iex> t = %PlymioAstVormFieldTest{}
      ...> {:ok, t} = t |> struct_vorm_reset
      ...> match?(nil, t.vorm)
      true

      iex> {:ok, vorm} = Plymio.Ast.Vorm.new(form: quote(do: x = x + 1))
      ...> {:ok, t} = %PlymioAstVormFieldTest{vorm: vorm}
      ...> |> struct_vorm_reset
      ...> match?(nil, t.vorm)
      true

  """

  @spec struct_vorm_reset(t) :: {:ok, t} | {:error, error}

  def struct_vorm_reset(%{@pav_key_struct_id => _} = state) do

    {:ok, state |> struct!([{@pav_key_vorm,  @pav_value_vorm_initial_value}])}

  end

  @doc ~S"""
  `struct_vorm_add/2` appends *new_forms* to the *vorm forms* of the *vorm* in the *vorm* field.

  `struct_vorm_ensure/1` is called first to ensure the *vorm field* is a *vorm*.

  If the `add` suceeds, `{:ok, state}` is returned.

  ## Examples

      iex> t = %PlymioAstVormFieldTest{}
      ...> {:ok, t} = t |> struct_vorm_add(quote(do: x = x + 1))
      ...> t.vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {8, ["x = x + 1"]}

      iex> t = %PlymioAstVormFieldTest{}
      ...> {:ok, t} = t |> struct_vorm_add(quote(do: x = x + 1))
      ...> {:ok, t} = t |> struct_vorm_add([quote(do: x = x * x), quote(do: x = x - 1)])
      ...> t.vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

  """

  @struct_var :state |> Macro.var(nil)
  @struct_match quote(do: %{@pav_key_struct_id => _} = unquote(@struct_var))
  @vorm_var :vorm |> Macro.var(nil)
  @other_vars 5 |> Macro.generate_arguments(nil)

  @struct_vorm_delegates_ok_state [

    {2, :struct_vorm_add, :add},
    {2, :struct_vorm_put, :put},
    {2, :struct_vorm_filter, :filter},
    {2, :struct_vorm_reject, :reject},
    {3, :struct_vorm_insert, :insert},
    {3, :struct_vorm_replace, :replace},
    {2, :struct_vorm_reduce, :reduce},
    {2, :struct_vorm_transform, :transform},
    {3, :struct_vorm_transform, :transform},
    {3, :struct_vorm_postwalk, :postwalk},
    {3, :struct_vorm_prewalk, :prewalk},
    {5, :struct_vorm_traverse, :traverse},
    {3, :struct_vorm_pipe_before, :pipe_before},
    {3, :struct_vorm_pipe_after, :pipe_after},

  ]

  for {arity, struct_fun, form_fun} <- @struct_vorm_delegates_ok_state do

    struct_args = [@struct_match | Enum.take(@other_vars,arity-1)]
    form_args = Enum.take(@other_vars, arity-1)

    def unquote(struct_fun)(unquote_splicing(struct_args)) do

      with {:ok, %{@pav_key_struct_id => _, @pav_key_vorm => %Plymio.Ast.Vorm{} = unquote(@vorm_var)} = unquote(@struct_var)} <- unquote(@struct_var) |> struct_vorm_ensure,
           {:ok, %Plymio.Ast.Vorm{} = unquote(@vorm_var)} <- unquote(@vorm_var) |> Plymio.Ast.Vorm.unquote(form_fun)(unquote_splicing(form_args))  do

        {:ok, unquote(@struct_var) |> struct!([{@pav_key_vorm, unquote(@vorm_var)}])}

      else
        {:error, _} = result -> result
      end

    end

  end

  @struct_vorm_delegates_ok_forms_state [
    {1, :struct_vorm_express, :express},
    {1, :struct_vorm_produce, :produce},

  ]

  for {arity, struct_fun, form_fun} <- @struct_vorm_delegates_ok_forms_state do

    struct_args = [@struct_match | Enum.take(@other_vars,arity-1)]
    form_args = Enum.take(@other_vars, arity-1)

    def unquote(struct_fun)(unquote_splicing(struct_args)) do

      with {:ok, %{@pav_key_struct_id => _, @pav_key_vorm => %Plymio.Ast.Vorm{} = unquote(@vorm_var)} = unquote(@struct_var)} <- unquote(@struct_var) |> struct_vorm_ensure,
           {:ok, {forms, %Plymio.Ast.Vorm{} = unquote(@vorm_var) = unquote(@vorm_var)}} <- unquote(@vorm_var) |> Plymio.Ast.Vorm.unquote(form_fun)(unquote_splicing(form_args))  do

        {:ok, {forms, unquote(@struct_var) |> struct!([{@pav_key_vorm, unquote(@vorm_var)}])}}

      else
        {:error, _} = result -> result
      end

    end

  end

  @struct_vorm_delegates_ok_forms [
    {2, :struct_vorm_fetch, :fetch},
  ]

  for {arity, struct_fun, form_fun} <- @struct_vorm_delegates_ok_forms do

    struct_args = [@struct_match | Enum.take(@other_vars,arity-1)]
    form_args = Enum.take(@other_vars, arity-1)

    def unquote(struct_fun)(unquote_splicing(struct_args)) do

      with {:ok, %{@pav_key_struct_id => _, @pav_key_vorm => %Plymio.Ast.Vorm{} = unquote(@vorm_var)}} <- unquote(@struct_var) |> struct_vorm_ensure,
           {:ok, _forms} = result <- unquote(@vorm_var) |> Plymio.Ast.Vorm.unquote(form_fun)(unquote_splicing(form_args))  do
        result
      else
        {:error, _} = result -> result
      end

    end

  end

end

