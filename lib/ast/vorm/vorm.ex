defmodule Plymio.Ast.Vorm do

  @moduledoc ~S"""
  The module provides a *toolkit* for Managing a Collection of Quoted Forms (Asts).

  Forms can be added, deleted, inserted, replaced, transformed, compiled, etc.

  ## Documentation Terms

  In the documentation below these terms, usually in *italics*, are used to mean the same thing (e.g. *vorm forms*).

  ### *form*

  A valid ast.

  ### *forms*

  One or more (`List`) of *form*s.

  ### *vorm*

  An instance of the module's state (create by e.g. `new/1`)

  ### *vorm forms*

  The *forms* held in the *vorm*.

  ### *form_range*

  The *forms* targetted by a function (e.g. `filter/2`).

  The *form_range* is normalised to a *predicate* function that, when passed a `{form, index}` tuple, returns either `true` or `false`.

  Here is a selection of valid form ranges. Note an `index` is zero-based and negative values are allowed.

  | Form Range | Forms Targetted |
  | :---       | :---            |
  | 0 | *first form* |
  | -1 | *last form* |
  | [2, 0] | *first and third forms* |
  | %{1 => :a, 3 => nil} | *second and fourth forms* |
  | nil | *all forms* |
  | `predicate_function` | when `true` |

  ### *new_forms*

  *new_forms* is used to refer to the new *forms* to add, insert, replace, etc.

  ### *form_index_transform*

  A pipeline of one or more arity one functions

  Each function in the pipeline will be passed a `{:form,index}` tuple and should return either `{:ok, form}` or `{:error, error}`.  Any other results is converted to `{:ok, form}`.  An error aborts the pipeline. The returned form is used create the `{transformed_form,index}` tuple to feed the next function in the pipeline.

  ### *form_walk_transform*

  A pipeline of one or more arity one functions

  A *form_walk_function* is an arity one function suitable as the second argument to `Macro.postwalk/2` or `Macro.prewalk/2`.

  Each *form* will be passed through sucessively each function in the *form_walk_transform* pipeline.

  ### *forms transform*

  A pipeline of one or more arity one functions

  Each function in the pipeline will be passed the current *forms* and should return `{:ok, forms}` or `{:error, error}`.  Any other results is converted to `{:ok, forms}`. The returned *forms* will be used to feed the next function in the pipeline. An error aborts the pipeline.

  ## Function Results

  Unless otherwise stated, the result of the function will be either `{:ok, vorm}` or `{:error, error}` where `error` is  an `Exception`.

  ## Vorm Struct

  The module's `struct` holds its state and has only one field:

  | Field | Purpose |
  | :---  | :---    |
  | `forms` | *the forms collection* |

  """

  import Kernel, except: [length: 1]
  alias Plymio.Ast.Utility, as: PAU
  alias Plymio.Ast.Vorm.Utility, as: PATU
  alias Plymio.Ast.Vorm.Utility.Pipeline, as: PAVUP

  use Plymio.Ast.Vorm.Attribute

  @type opts :: Keyword.t
  @type t :: %__MODULE__{}
  @type kv :: {any,any}
  @type error :: struct
  @type form :: Macro.t
  @type forms :: [form]
  @type items :: list

  import Plymio.Ast.Vorm.Vormen, only: [
    vormen_create_transform: 3,
    vormen_create_transform: 4,
    vormen_validate: 1,
  ]

  import Plymio.Ast.Vorm.Vormen.Transform, only: [
    normalise_form_index_transform: 1,
    normalise_form_postwalk_transform: 1,
    normalise_form_prewalk_transform: 1,
    normalise_form_traverse_transform: 3,
    normalise_vormen_transform: 1,
  ]

  @doc false
  defdelegate forms_pipe(asts), to: PAU
  @doc false
  defdelegate forms_reduce(form), to: PAU
  @doc false
  defdelegate form_validate(form), to: PAU

  @pav_form_defstruct [
    {@pav_key_forms, @pav_value_forms_initial_value},
  ]

  @pav_form_struct_kvs_aliases [

    @pav_alias_forms,
  ]

  @pav_form_struct_dict_aliases @pav_form_struct_kvs_aliases
  |> PATU.opts_create_aliases_dict

  @doc false

  def opts_canon_keys!(opts, dict \\ @pav_form_struct_dict_aliases) do
    opts |> PATU.opts_canon_keys!(dict)
  end

  defstruct @pav_form_defstruct

  @doc ~S"""
  `new/1` is used to create a new *vorm*

  ## Examples

      iex> {:ok, vorm} = new()
      ...> vorm |> helper_vorm_test_forms!
      {nil, []}

  The forms can be populated at the same time:

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {15, ["x = x + 1", "x = x * x", "x = x - 1"]}

  """

  @spec new(opts) :: {:ok, t} | {:error, error}

  def new(opts \\ [])

  def new([]) do
    {:ok, %__MODULE__{}}
  end

  def new(opts) do
    with {:ok, %__MODULE__{} = vorm} <- new() do
      vorm |> update(opts)
    else
      {:error, _} = result -> result
    end
  end

  @doc ~S"""
  `new!/1` calls `new/1` and if the result is `{:ok, vorm}` returns the `vorm`. Otherwise the `error` in `{:error, error}` is raised.

  ## Examples

      iex> vorm = new!()
      ...> vorm |> helper_vorm_test_forms!
      {nil, []}

  The forms can be populated at the same time:

      iex> vorm = new!(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {15, ["x = x + 1", "x = x * x", "x = x - 1"]}

  """

  @spec new!(opts) :: t | no_return

  def new!(opts \\ []) do

    opts
    |> new()
    |> case do
         {:ok, %__MODULE__{} = state} -> state
         {:error, error} -> raise error
       end

  end

  @spec update_field(t, kv) :: {:ok, t} | {:error, error}

  defp update_field(state, kv)

  defp update_field(%__MODULE__{} = state, {k,new_forms})
  when k in [
    @pav_key_forms,
  ] do

    new_forms
    |> case do

         @pav_value_forms_initial_value ->

           {:ok, state |> struct!([{@pav_key_forms, @pav_value_forms_initial_value}])}

         new_forms ->

           with {:ok, new_forms} <- new_forms |> vormen_validate do
             {:ok, state |> struct!([{@pav_key_forms, new_forms}])}
           else
             {:error, _} = result -> result
           end

       end

  end

  @spec update(t, opts) :: {:ok, t} | {:error, error}

  defp update(vorm, opts)

  defp update(%__MODULE__{} = state, []) do
    {:ok, state}
  end

  defp update(%__MODULE__{} = state, opts) when is_list(opts) do

    opts
    |> opts_canon_keys!
    |> Enum.reduce_while(state, fn {k,v}, s ->

      s
      |> update_field({k,v})
      |> case do
           %__MODULE__{} = s -> {:cont, s}
           {:ok, %__MODULE__{} = s} -> {:cont, s}
           {:error, error} -> {:halt, error}
         end

    end)
    |> case do
         {:error, _} = result -> result
         %__MODULE__{} = vorm -> {:ok, vorm}
       end

  end

  @doc false

  @spec vorm_update_forms(t, any) :: {:ok, t} | {:error, error}

  def vorm_update_forms(%__MODULE__{} = state, new_forms) do
    state |> update([{@pav_key_forms, new_forms}])
  end

  @doc false

  @spec express(t) :: {:ok, {forms, t}} | {:error, error}

  def express(%__MODULE__{@pav_key_forms => forms} = state) do
    {:ok, {forms, state}}
  end

  @doc false

  @spec produce(t) :: {:ok, {forms, t}} | {:error, error}
  @spec produce(opts) :: {:ok, {forms, t}} | {:error, error}

  def produce(vorm)

  def produce(%__MODULE__{} = state) do

    with {:ok, {_forms, %__MODULE__{}}} = result <- state |> express  do
      result
    else
      {:error, _} = result -> result
    end

  end

  def produce(opts) when is_list(opts) do

    with {:ok, %__MODULE__{} = vorm} <- opts |> new(),
         {:ok, {_forms, %__MODULE__{}}} = result <- vorm |> produce do
      result
    else
      {:error, _} = result -> result
    end

  end

  @doc false

  defmacro reify(opts \\ []) do

    quote bind_quoted: [
      opts: opts
    ] do

      with {:ok, {forms, %Plymio.Ast.Vorm{}}} <- opts |> Plymio.Ast.Vorm.produce() do

        forms
        |> Code.eval_quoted([], __ENV__)

      else
        {:error, error} -> raise error
      end

    end

  end

  @doc ~S"""
  `produce_pipeline/2` take a list (often a `Keyword`) of `{verb,args}` tuples and an (optional) *vorm*.

  If the *vorm* is `nil`, a new, empty one is created.

  Each `verb` must be the name of one of the functions that return `{:ok, vorm}` on sucess (e.g. `add`).

  Each `{verb, args}` tuple is applied to the *vorm* (using `Enum.reduce/3`).

  The result on sucess will be `{:ok, {forms, vorm}}`

  Each `verb` has aliases:

  | Canononical Verb | Verb Aliases |
  | :---------------- | -------------------: |
  | `:add` | *:add_forms, :add_form* |
  | `:filter` | *:filter_forms, :filter_form* |
  | `:insert` | *:insert_forms, :insert_form* |
  | `:pipe_before` | *:pipe_before_forms, :pipe_before_form* |
  | `:pipe_after` | *:pipe_after_forms, :pipe_after_form* |
  | `:postwalk` | *:postwalk_forms, :postwalk_form* |
  | `:prewalk` | *:prewalk_forms, :prewalk_form* |
  | `:put` | *:put_forms, :put_form* |
  | `:reject` | *:reject_forms, :reject_form* |
  | `:reduce` | *:reduce_forms, :reduce_form* |
  | `:replace` | *:replace_forms, :replace_form* |
  | `:transform` | *:transform_forms, :transform_form* |
  | `:traverse` | *:traverse_forms, :traverse_form* |

  ## Examples

  These initial two examples demonstrate the *vorm* being created automatically if not given as the second argument.

      iex> {:ok, {_forms, vorm}} = produce_pipeline(add: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

      iex> {:ok, {_forms, vorm}} = produce_pipeline(
      ...>  add: quote(do: x = x + 1),
      ...>  add_forms: [quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

   This example uses an existing *vorm*

      iex> {:ok, vorm} = new(form: quote(do: x = x + 1))
      ...> {:ok, {_forms, vorm}} = produce_pipeline(
      ...>  [add_forms: [quote(do: x = x * x), quote(do: x = x - 1)]],
      ...>  vorm)
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

   Combining different actions (e.g. `:add`, `:insert`, etc.

      iex> {:ok, {_forms, vorm}} = produce_pipeline(
      ...>  add_form: quote(do: x = x * x),
      ...>  insert_form: [0, quote(do: x = x + 1)],
      ...>  insert: [:append, quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

      iex> {:ok, {_forms, vorm}} = produce_pipeline(
      ...>  add: quote(do: x = x * x),
      ...>  insert: [0, quote(do: x = x + 1)],
      ...>  insert: [:append, quote(do: x = x - 1)],
      ...>  postwalk: [nil,
      ...>    fn
      ...>     {:x, _, m} when is_atom(m) -> quote(do: y)
      ...>     x -> x
      ...>    end])
      ...> vorm |> helper_vorm_test_forms!(binding: [y: 11])
      {143, ["y = y + 1", "y = y * y", "y = y - 1"]}

      iex> {:ok, {_forms, vorm}} = produce_pipeline(add: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)],
      ...>   put: quote(do: x = x * x * x))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {27, ["x = x * x * x"]}

   Values that are not 2tuples will be intepreted as an `add`.

      iex> {:ok, {_forms, vorm}} = produce_pipeline([
      ...>   quote(do: x = x + 1),
      ...>   quote(do: x = x * x),
      ...>   quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

   Using mix *form* and explicit `{verb,args}` syntax:

      iex> {:ok, {_forms, vorm}} = produce_pipeline([
      ...>  quote(do: x = x + 1),
      ...>  {:add, [quote(do: x = x * x), quote(do: x = x - 1)]},
      ...>  {:insert_form, [:append, quote(do: x = x * x * x)]}])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {3375, ["x = x + 1", "x = x * x", "x = x - 1", "x = x * x * x"]}

   Using mixed *form* and `Keyword` `verb: args` syntax:

      iex> {:ok, {_forms, vorm}} = produce_pipeline([
      ...>  quote(do: x = x + 1),
      ...>  add: [quote(do: x = x * x), quote(do: x = x - 1)],
      ...>  insert: [:append, quote(do: x = x * x * x)]])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {3375, ["x = x + 1", "x = x * x", "x = x - 1", "x = x * x * x"]}

      iex> {:ok, {_forms, vorm}} = produce_pipeline([
      ...>  quote(do: x = x + 1),
      ...>  [add_forms: [quote(do: x = x * x), quote(do: x = x - 1)]],
      ...>  [insert: [:append, quote(do: x = x * x * x)]]])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {3375, ["x = x + 1", "x = x * x", "x = x - 1", "x = x * x * x"]}

   The verb here (`:add_typo`) is not known and a `KeyError` is returned:

      iex> {:error, %KeyError{} = error} = produce_pipeline(add_typo: quote(do: x = x + 1))
      ...> error.key
      :add_typo

  """

  @spec produce_pipeline(any, any) :: {:ok, {forms, t}} | {:error, error}

  def produce_pipeline(opts, vorm \\ nil)

  def produce_pipeline(opts, nil) when is_list(opts) do

    with {:ok, vorm} <- new() do
      opts |> produce_pipeline(vorm)
    else
      {:error, _} = result -> result
    end

  end

  def produce_pipeline([], %__MODULE__{} = state) do
    {:ok, state}
  end

  def produce_pipeline(opts, %__MODULE__{} = state) when is_list(opts) do

    with {:ok, %__MODULE__{} = state} <- state |> PAVUP.pipeline(opts),
         {:ok, {_forms, %__MODULE__{}}} = result <- state |> express do
      result
    else
      {:error, _} = result -> result
    end

  end

  def produce_pipeline(opts, new_opts) when is_list(opts) and is_list(new_opts) do

    with {:ok, %__MODULE__{} = state} <- new_opts |> new,
         {:ok, {_forms, %__MODULE__{}}} = result <- opts |> produce_pipeline(state) do
      result
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `reify_pipeline/2` is a macro that calls `produce_pipeline/2` and compiles the `forms` from the sucess result `{:ok, {forms, vorm}}`.

  """

  defmacro reify_pipeline(pipeline, opts \\ []) do

    quote bind_quoted: [
      pipeline: pipeline,
      opts: opts
    ] do

      with {:ok, {forms, %Plymio.Ast.Vorm{}}} <- pipeline |> Plymio.Ast.Vorm.produce_pipeline(opts) do

        forms
        |> Code.eval_quoted([], __ENV__)

      else
        {:error, error} -> raise error
      end

    end

  end

  ### want collate to pull form_range of forms, unquote splicign and put to 2dn form_range; default append.

  @doc ~S"""
  `reduce/2` take a *vorm* and a *forms transform* and passes the *vorm forms* to the normalised *forms transform*, expecting `{:ok, reduced_forms}` as the result.

  The `reduced_forms` are used to update the *vorm forms*.

  ## Examples

  The indentity reduction does nothing:

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...>  {:ok, collate_vorm} = vorm |> reduce(&(&1))
      ...> collate_vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

  This example uses `Plymio.Ast.Utility.forms_reduce/1` to create a single *form* from the *vorm forms*.

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...>  {:ok, collate_vorm} = vorm |> reduce(&Plymio.Ast.Utility.forms_reduce/1)
      ...> collate_vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["(x = x + 1\n x = x * x\n x = x - 1)"]}

  """

  @spec reduce(any, any) :: {:ok, t} | {:error, error}

  def reduce(vorm, form_reduce)

  def reduce(%__MODULE__{@pav_key_forms => forms} = state, reduce) do

    with {:ok, fun_reduce} <- reduce |> normalise_vormen_transform do

      forms
      |> fun_reduce.()
      |> case  do
           {:error, _} = result -> result
           {:ok, _} = result -> result
           forms -> {:ok, forms}
         end
         |> case do
              {:error, _} = result -> result
              {:ok, forms} -> state |> vorm_update_forms(forms)
            end

    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `transform/2` take a *vorm*, *form_range* and a *form_index_transform*.

  The normalised *form_index_transform* is applied to each *form* in the *vorm forms*.

  ## Examples

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> {:ok, vorm} = vorm |> transform(
      ...>   1, fn {_form,_index} -> quote(do: x = x * x * x) end)
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {63, ["x = x + 1", "x = x * x * x", "x = x - 1"]}

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...>  {:ok, vorm} = vorm |> transform(
      ...>    nil, fn {_form,index} -> quote(do: y = y + unquote(index)) end)
      ...> vorm |> helper_vorm_test_forms!(binding: [y: 5])
      {8, ["y = y + 0", "y = y + 1", "y = y + 2"]}

  """

  @spec transform(any, any, any) :: {:ok, t} | {:error, error}

  def transform(vorm, form_range, form_transform \\ [])

  def transform(state, _form_range, []) do
    {:ok, state}
  end

  def transform(%__MODULE__{@pav_key_forms => forms} = state, form_range, transform) do

    with {:ok, fun_transform} <- transform |> normalise_form_index_transform do

      fun_then = fn fi, _new_forms, forms ->

        fi
        |> fun_transform.()
        |> case do
             {:error, _} = result -> result
             {:ok, new_form} -> forms ++ [new_form]
             new_form -> forms ++ [new_form]
           end

      end

      fun_else = fn {form,_index}, _new_forms, forms ->
        {:ok, forms ++ [form]}
      end

      transform_opts = [
        {@pav_key_fun_then, fun_then},
        {@pav_key_fun_else, fun_else},

      ]

      with {:ok, new_forms} <- forms |> vormen_create_transform(form_range,
        @pav_transform_type_transform, transform_opts) do
        state |> vorm_update_forms(new_forms)
      else
        {:error, _} = result -> result
      end

    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `postwalk/2` take a *vorm*, *form_range* and *form_walk_transform*.

  The *form_walk_transform* is applied to each *form* in the *form_range* using `Macro.postwalk/2`.

  ## Examples

      iex> {:ok, vorm} = new(form: quote(do: z = x + y))
      ...> {:ok, vorm} = vorm |> postwalk(0, fn
      ...>     {:x, _, m} when is_atom(m) -> quote(do: a)
      ...>     {:y, _, m} when is_atom(m) -> quote(do: b)
      ...>     x -> x
      ...>   end)
      ...> vorm |> helper_vorm_test_forms!(binding: [a: 39, b: 3])
      {42, ["z = a + b"]}

  Here two transforms are applied:

      iex> {:ok, vorm} = new(form: quote(do: z = x + y))
      ...> {:ok, vorm} = vorm |> postwalk(0, [
      ...>   fn
      ...>     {:x, _, m} when is_atom(m) -> quote(do: a)
      ...>     {:y, _, m} when is_atom(m) -> quote(do: b)
      ...>     x -> x
      ...>   end,
      ...>   fn
      ...>     {:a, _, m} when is_atom(m) -> quote(do: p)
      ...>     {:b, _, m} when is_atom(m) -> quote(do: q)
      ...>     x -> x
      ...>   end])
      ...> vorm |> helper_vorm_test_forms!(binding: [p: 20, q: 22])
      {42, ["z = p + q"]}

  """

  @spec postwalk(any, any, any) :: {:ok, t} | {:error, error}

  def postwalk(vorm, form_range, form_postwalk \\ [])

  def postwalk(state, _form_range, []) do
    {:ok, state}
  end

  def postwalk(%__MODULE__{} = state, form_range, postwalk) do

    with {:ok, fun_postwalk} <- postwalk |> normalise_form_postwalk_transform,
         {:ok, %__MODULE__{}} = result <- state |> transform(form_range, fun_postwalk) do
      result
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `prewalk/2` take a *vorm*, *form_range* and *form_walk_transform*.

  The *form_walk_transform* is applied to each *form* in the *form_range* using `Macro.prewalk/2`.

  ## Examples

      iex> {:ok, vorm} = new(form: quote(do: z = x + y))
      ...> {:ok, vorm} = vorm |> prewalk(0, fn
      ...>     {:x, _, m} when is_atom(m) -> quote(do: a)
      ...>     {:y, _, m} when is_atom(m) -> quote(do: b)
      ...>     x -> x
      ...>   end)
      ...> vorm |> helper_vorm_test_forms!(binding: [a: 39, b: 3])
      {42, ["z = a + b"]}

  Here two transforms are applied:

      iex> {:ok, vorm} = new(form: quote(do: z = x + y))
      ...> {:ok, vorm} = vorm |> prewalk(0, [
      ...>   fn
      ...>     {:x, _, m} when is_atom(m) -> quote(do: a)
      ...>     {:y, _, m} when is_atom(m) -> quote(do: b)
      ...>     x -> x
      ...>   end,
      ...>   fn
      ...>     {:a, _, m} when is_atom(m) -> quote(do: p)
      ...>     {:b, _, m} when is_atom(m) -> quote(do: q)
      ...>     x -> x
      ...>   end])
      ...> vorm |> helper_vorm_test_forms!(binding: [p: 20, q: 22])
      {42, ["z = p + q"]}

  """

  @spec prewalk(any, any, any) :: {:ok, t} | {:error, error}

  def prewalk(vorm, form_range, form_prewalk \\ [])

  def prewalk(state, _form_range, []) do
    {:ok, state}
  end

  def prewalk(%__MODULE__{} = state, form_range, prewalk) do

    with {:ok, fun_prewalk} <- prewalk |> normalise_form_prewalk_transform,
         {:ok, %__MODULE__{}} = result <- state |> transform(form_range, fun_prewalk) do
      result
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `traverse/2` take a *vorm*, *form_range*, the initial traverse accummulator, the **pre** *form_traverse_transform* and the **post** *form_traverse_transform*.

  `Macro.traverse/4` returns `{new_form, acc}`; `new_form` is used to replace the original one and `acc` is ignored.

  ## Examples

  This contrived example uses the *pre* stage to change the `x` and `y` vars to `a` and `b`, and the *post* stage to change `value1` and `value2` to their values in the accumulator (a lookup dictionary).

      iex> {:ok, vorm} = new(form: quote do
      ...>   x = value1
      ...>   y = value2
      ...>   z = x + y
      ...> end)
      ...> {:ok, vorm} = vorm |> traverse(-1,
      ...>   # the accumulator is a lookup dictionary
      ...>   %{value1: 39, value2: 3},
      ...>   # pre function
      ...>   fn
      ...>     # x to a
      ...>     {:x, _, _}, acc -> {Macro.var(:a,nil), acc}
      ...>     # y to b
      ...>     {:y, _, _}, acc -> {Macro.var(:b,nil), acc}
      ...>     # passthru
      ...>     ast, acc -> {ast, acc}
      ...>   end,
      ...>   # post function
      ...>   fn
      ...>     # is the var (form) in the acc lookup table? if so replace with value
      ...>     {form, _, mod} = ast, acc when is_atom(form) and is_atom(mod) ->
      ...>        case acc |> Map.has_key?(form) do
      ...>          true -> {Map.get(acc,form), acc}
      ...>          _ -> {ast, acc}
      ...>        end
      ...>     # passthru
      ...>     ast, acc -> {ast, acc}
      ...>   end)
      ...> vorm |> helper_vorm_test_forms!(binding: [])
      {42, ["(a = 39\n b = 3\n z = a + b)"]}

  """

  @spec traverse(any, any, any, any, any) :: {:ok, t} | {:error, error}

  def traverse(vorm, form_range, acc_traverse, pre_traverse, post_traverse)

  def traverse(%__MODULE__{} = state, form_range, acc_traverse, pre_traverse, post_traverse) do

    with {:ok, traverse_transform} <- normalise_form_traverse_transform(acc_traverse, pre_traverse, post_traverse),
         {:ok, %__MODULE__{}} = result <- state |> transform(form_range, traverse_transform) do
      result
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `length/1` returns the number of *vorm forms*.

  ## Examples

      iex> {:ok, vorm} = new()
      ...> vorm |> Plymio.Ast.Vorm.length
      0

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> Plymio.Ast.Vorm.length
      3

  """

  @spec length(t) :: integer

  def length(vorm)

  def length(%__MODULE__{@pav_key_forms => forms}) do

    forms |> Kernel.length

  end

  @doc ~S"""
  `empty?/1` takes a *vorm* and returns `true` if the *vorm forms* is empty, else `false`.

  ## Examples

      iex> {:ok, vorm} = new()
      ...> vorm |> Plymio.Ast.Vorm.empty?
      true

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> Plymio.Ast.Vorm.empty?
      false

  """

  @spec empty?(t) :: true | false

  def empty?(vorm)

  def empty?(%__MODULE__{@pav_key_forms => forms}) do
    case forms |> Kernel.length do
      0 -> true
      _ -> false
    end
  end

  @doc ~S"""
  `add/2` appends *new_forms* to the *vorm forms*.

  ## Examples

      iex> {:ok, vorm} = new!() |> add(quote(do: x = x + 1))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {8, ["x = x + 1"]}

      iex> {:ok, vorm} = new!() |> add(quote(do: x = x + 1))
      ...> {:ok, vorm} = vorm |> add([quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

      iex> {:ok, vorm} = new!() |> add([
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {15, ["x = x + 1", "x = x * x", "x = x - 1"]}

      iex> {:ok, vorm} = new!() |> add(nil)
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {nil, []}

      iex> {:ok, vorm} = new!() |> add([])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {nil, []}

  """

  @spec add(t, any) :: {:ok, t} | {:error, error}

  def add(vorm, new_forms)

  def add(%__MODULE__{@pav_key_forms => forms} = state, new_forms) do

    with {:ok, new_forms} <- new_forms |> vormen_validate do
      state |> vorm_update_forms(forms ++ new_forms)
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `put/2` completely replaces the *vorm forms* with *new_forms*

  ## Examples

      iex> {:ok, vorm} = new(form: (quote(do: x = x + 1)))
      ...> {:ok, vorm} = vorm |> put([quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {48, ["x = x * x", "x = x - 1"]}

      iex> {:ok, vorm} = new!() |> put([
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {15, ["x = x + 1", "x = x * x", "x = x - 1"]}

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...>  {:ok, put_vorm} = vorm |> put(quote(do: x = 42))
      ...> put_vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {42, ["x = 42"]}

  """

  @spec put(t, any) :: {:ok, t} | {:error, error}

  def put(vorm, new_forms)

  def put(%__MODULE__{} = state, new_forms) do

    with {:ok, forms} <- new_forms |> vormen_validate do
      state |> vorm_update_forms(forms)
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `fetch/3` take a *vorm* and *form_range* and returns the forms in the *form_range* as `{:ok, forms}`.

  The default *form_range* is `nil` i.e. return all forms.

  ## Examples

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> {:ok, forms} = vorm |> fetch(1)
      ...> {:ok, fetch_vorm} = new(forms: forms)
      ...> fetch_vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {49, ["x = x * x"]}

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> {:ok, forms} = vorm |> fetch([0,-1])
      ...> {:ok, fetch_vorm} = new(forms: forms)
      ...> fetch_vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {7, ["x = x + 1", "x = x - 1"]}

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> {:ok, forms} = vorm |> fetch
      ...> {:ok, fetch_vorm} = new(forms: forms)
      ...> fetch_vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {63, ["x = x + 1", "x = x * x", "x = x - 1"]}

  """

  @spec fetch(t, any) :: {:ok, forms} | {:error, error}

  def fetch(vorm, form_range \\ nil)

  def fetch(%__MODULE__{@pav_key_forms => forms}, form_range) do

    with {:ok, _forms} = result <- forms |> vormen_create_transform(form_range, @pav_transform_type_fetch) do
      result
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `filter/2` takes a *vorm* and *form_range* and filters *only* those forms in the *form_range*

  ## Examples

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...>  {:ok, vorm} = vorm |> filter([-1,0])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {7, ["x = x + 1", "x = x - 1"]}

  """

  @spec filter(t, any) :: {:ok, t} | {:error, error}

  def filter(vorm, form_range)

  def filter(%__MODULE__{@pav_key_forms => forms} = state, form_range) do

    with {:ok, new_forms} <- forms |> vormen_create_transform(form_range, @pav_transform_type_filter) do
      state |> vorm_update_forms(new_forms)
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `reject/2` takes a *vorm* and *form_range* and deletes all forms in the *form_range*.

  ## Examples

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...>  {:ok, vorm} = vorm |> reject([0,2])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {49, ["x = x * x"]}

  """

  @spec reject(t, any) :: {:ok, t} | {:error, error}

  def reject(vorm, form_range)

  def reject(%__MODULE__{@pav_key_forms => forms} = state, form_range) do

    with {:ok, new_forms} <- forms |> vormen_create_transform(form_range, @pav_transform_type_reject) do
      state |> vorm_update_forms(new_forms)
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `insert/3` take a *vorm*, *form_range* and *new_forms*.

  The *new_forms* are *spliced* into the *vorm forms* at **each** index in the *form_range*.

  As a special case, if the *form_range* is `:append`, the *new_forms* are *appended* (i.e. `add/2`)

  ## Examples

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> {:ok, vorm} = vorm |> insert(2, quote(do: x = x * x * x))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {262143, ["x = x + 1", "x = x * x", "x = x * x * x", "x = x - 1"]}

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> {:ok, vorm} = vorm |> insert(nil, quote(do: x = x + 42))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {8505, ["x = x + 42", "x = x + 1", "x = x + 42", "x = x * x", "x = x + 42", "x = x - 1"]}

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...> {:ok, vorm} = vorm |> insert(:append, quote(do: x = x + 42))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {105, ["x = x + 1", "x = x * x", "x = x - 1", "x = x + 42"]}

   Inserting into an empty *vorm* with *form_range* `nil` is same as `:append`

      iex> {:ok, vorm} = new()
      ...> {:ok, vorm} = vorm |> insert(nil, quote(do: x = x * x * x))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 3])
      {27, ["x = x * x * x"]}

  """

  @spec insert(t, any, any) :: {:ok, t} | {:error, error}

  def insert(vorm, form_range, new_forms)

  def insert(%__MODULE__{} = state, form_range, new_forms)
  when form_range in [@pav_key_append] do

    state |> add(new_forms)

  end

  def insert(%__MODULE__{@pav_key_forms => forms} = state, nil, new_forms)
    when Kernel.length(forms) == 0 do

    state |> insert(@pav_key_append, new_forms)

  end

  def insert(%__MODULE__{@pav_key_forms => forms} = state, form_range, new_forms) do

    with {:ok, new_forms} <- new_forms |> vormen_validate,
         {:ok, forms} <- forms
         |> vormen_create_transform(form_range, @pav_transform_type_insert, new_forms: new_forms) do
      state |> vorm_update_forms(forms)
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `replace/3` take a *vorm*, *form_range* and *new_forms*.

  The *new_forms* are *spliced* into the *vorm forms* at **each** index in the *form_range*, replacing the existing *form*.

  ## Examples

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...>  {:ok, vorm} = vorm |> replace(1, quote(do: x = x * x * x))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {511, ["x = x + 1", "x = x * x * x", "x = x - 1"]}

      iex> {:ok, vorm} = new(forms: [
      ...>   quote(do: x = x + 1), quote(do: x = x * x), quote(do: x = x - 1)])
      ...>  {:ok, vorm} = vorm |> replace([0,-1], quote(do: x = x * x * x))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {1628413597910449, ["x = x * x * x", "x = x * x", "x = x * x * x"]}

  """

  @spec replace(t, any, any) :: {:ok, t} | {:error, error}

  def replace(vorm, form_range, new_forms)

  def replace(%__MODULE__{@pav_key_forms => forms} = state, form_range, new_forms) do

    with {:ok, new_forms} <- new_forms |> vormen_validate,
         {:ok, new_forms} <- forms
         |> vormen_create_transform(form_range, @pav_transform_type_replace, new_forms: new_forms) do
      state |> vorm_update_forms(new_forms)
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `pipe_before/2` take a *vorm*, *form_range* and *new_forms*.

  Each *form* in the *form_range* is piped *before* the *new_forms*.

  This example pipes a var (`x`) into a call to an anonymous function that adds 1:

      iex> {:ok, vorm} = new(forms: quote(do: x))
      ...> {:ok, vorm} = vorm |> pipe_before(0, quote(do: fn v -> v + 1 end.()))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 42])
      {43, ["(fn v -> v + 1 end).(x)"]}

  This example selects the last (`-`) form to be before a list of forms.

      iex> before_ast1 = quote(do: fn v -> v + 1 end.())
      ...> before_ast2 = quote(do: fn v -> v * v end.())
      ...> before_ast3 = quote(do: fn v -> 1 - v end.())
      ...> {:ok, vorm} = new(forms: [quote(do: x = x + 5), quote(do: x)])
      ...> {:ok, vorm} = vorm |> pipe_before(-1, [before_ast1, before_ast2, before_ast3])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 37])
      {-1848, ["x = x + 5", "(fn v -> 1 - v end).((fn v -> v * v end).((fn v -> v + 1 end).(x)))"]}

  Implicit in the examples above has been that the left side form of
  the pipe (`|>`) should become the zeroth argument of the right side
  form. However the call to `Macro.pipe/3` that does the piping takes
  the index (zero offset integer) to use (in the example above it was set automatically to zero).

  To specify the *pipe* index, *any* of the asts can be a 2tuple where the first element is the "pure" ast and the second the pipe index.

  > Note the *pipe* index is unrelated to the *form* index.

  These two simple examples of a subtraction show why the pipe index
  is important: The first uses the default index of zero (so the code
  becomes `x - 100`) while the second supplies an index of 1 (so the code
  becomes `100 - x`).

      iex> before_ast = quote(do: Kernel.-(100))
      ...> {:ok, vorm} = new(form: quote(do: x))
      ...> {:ok, vorm} = vorm |> pipe_before(-1, before_ast)
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 42])
      {-58, ["Kernel.-(x, 100)"]}

      iex> before_ast = {quote(do: Kernel.-(100)), 1}
      ...> {:ok, vorm} = new(form: quote(do: x))
      ...> {:ok, vorm} = vorm |> pipe_before(-1, before_ast)
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 42])
      {58, ["Kernel.-(100, x)"]}

  This somewhat complicated example demonstrates using multiple asts with indicies:

      iex> {:ok, vorm} = new(form: Macro.var(:x, nil))
      ...> {:ok, vorm} = vorm |> pipe_before(-1, [
      ...>   {quote(do: Kernel./(42)), 1}, # index is 1
      ...>    quote(do: List.wrap),
      ...>   {quote(do: Kernel.++([1,2,3])), 1}, # index is 1
      ...>   {quote(do: Enum.reduce([&Enum.sum/1, fn v -> v * v end], fn f, s -> f.(s) end)), 1},
      ...>   ])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 7])
      {144.0, ["Enum.reduce([&Enum.sum/1, fn v -> v * v end], Kernel.++([1, 2, 3], List.wrap(Kernel./(42, x))), fn f, s -> f.(s) end)"]}

  """

  @spec pipe_before(t, any, any) :: {:ok, t} | {:error, error}

  def pipe_before(vorm, form_range, new_forms)

  def pipe_before(%__MODULE__{@pav_key_forms => forms} = state, form_range, new_forms) do

    with {:ok, new_forms} <- new_forms |> vormen_validate,
         {:ok, new_forms} <- forms
         |> vormen_create_transform(form_range, @pav_transform_type_pipe_before, new_forms: new_forms) do
      state |> vorm_update_forms(new_forms)
    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""
  `pipe_after/2` take a *vorm*, *form_range* and *new_forms*.

  Each *form* in the *form_range* is piped *after* the *new_forms*.

  This is the same example as above that pipes a var (`x`) into a call
  to an anonymous function that adds 1 *but* the arguments are
  switched around:

      iex> {:ok, vorm} = new(form: quote(do: fn v -> v + 1 end.()))
      ...> {:ok, vorm} = vorm |> pipe_after(0, quote(do: x))
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 42])
      {43, ["(fn v -> v + 1 end).(x)"]}

  Again, the same example as for `pipe_before` that supplies a list of asts to the
  but with the arguments switched around:

      iex> after_ast1 = Macro.var(:x, nil)
      ...> after_ast2 = quote(do: fn v -> v + 1 end.())
      ...> after_ast3 = quote(do: fn v -> v * v end.())
      ...> {:ok, vorm} = new(form: quote(do: fn v -> 1 - v end.()))
      ...> {:ok, vorm} = vorm |> pipe_after(-1, [after_ast1, after_ast2, after_ast3])
      ...> vorm |> helper_vorm_test_forms!(binding: [x: 42])
      {-1848, ["(fn v -> 1 - v end).((fn v -> v * v end).((fn v -> v + 1 end).(x)))"]}

  """

  @spec pipe_after(t, any, any) :: {:ok, t} | {:error, error}

  def pipe_after(vorm, form_range, new_forms)

  def pipe_after(%__MODULE__{@pav_key_forms => forms} = state, form_range, new_forms) do

    with {:ok, new_forms} <- new_forms |> vormen_validate,
         {:ok, new_forms} <- forms
         |> vormen_create_transform(form_range, @pav_transform_type_pipe_after, new_forms: new_forms) do
      state |> vorm_update_forms(new_forms)
    else
      {:error, _} = result -> result
    end

  end

end

defimpl Inspect, for: Plymio.Ast.Vorm do

  use Plymio.Ast.Vorm.Attribute

  def inspect(%Plymio.Ast.Vorm{@pav_key_forms => forms}, _opts) do

    forms_telltale =
      case forms do
        x when is_nil(x) -> "X"
        x when is_list(x) -> "#{inspect length(x)}"
      end

   "PAV(#{forms_telltale})"

  end

end

