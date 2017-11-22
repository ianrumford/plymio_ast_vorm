defmodule Plymio.Ast.Vorm.Mixfile do
  use Mix.Project

  @version "0.1.0"

  def project do
    [app: :plymio_ast_vorm,
     version: @version,
     description: description(),
     package: package(),
     source_url: "https://github.com/ianrumford/plymio_ast_vorm",
     homepage_url: "https://github.com/ianrumford/plymio_ast_vorm",
     docs: [extras: ["./README.md", "./CHANGELOG.md"]],
     elixirc_paths: elixirc_paths(Mix.env),
     elixir: "~> 1.5",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:plymio_option, "~> 0.3.0"},
      {:plymio_ast, "~> 1.0.0"},
      {:ex_doc, "~> 0.18", only: :dev}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/helper"]
  defp elixirc_paths(_),     do: ["lib"]

  defp package do
    [maintainers: ["Ian Rumford"],
     files: ["lib", "mix.exs", "README*", "LICENSE*", "CHANGELOG*"],
     licenses: ["MIT"],
     links: %{github: "https://github.com/ianrumford/plymio_ast_vorm"}]
  end

  defp description do
    """
    plymio_ast_vorm: Managing a Collection of Quoted Forms (Asts)
    """
  end

end
