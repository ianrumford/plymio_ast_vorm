defmodule Plymio.Ast.Vorm.Utility do

  @moduledoc false

  require Plymio.Option.Utility, as: POU
  alias Plymio.Ast.Vorm.Error, as: PAVE
  alias Plymio.Ast.Utility, as: PAU
  use Plymio.Ast.Vorm.Attribute

  defdelegate new_error(opts), to: PAVE, as: :new
  defdelegate new_error_result(opts), to: PAVE, as: :new_result

  defdelegate opts_canon_keys!(opts,dict), to: POU, as: :opts_canonical_keys!
  defdelegate opts_create_aliases_dict(opts), to: POU, as: :opts_create_aliases_dict

  defdelegate list_wrap_flat_just(value), to: POU
  defdelegate list_wrap_flat_just_uniq(value), to: POU

  defdelegate form_validate(form), to: PAU
  defdelegate forms_validate(form), to: PAU

end

