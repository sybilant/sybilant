{ pkgs, ... }:

let
  vale = pkgs.vale.withStyles (styles: [ styles.google ]);
in
{
  languages.typst = {
    enable = true;
    fontPaths = [
      "${pkgs.libertinus}/share/fonts"
      "${pkgs.source-sans}/share/fonts"
      "${pkgs.source-code-pro}/share/fonts"
    ];
  };

  packages = [
    pkgs.entr
    pkgs.graphviz
    vale
  ];

  scripts.build-design = {
    exec = ''
      exec "$DEVENV_ROOT/bin/build-design" "$@"
    '';
    description = "Build the Sybilant design document";
  };

  scripts.watch = {
    exec = ''
      exec "$DEVENV_ROOT/bin/watch" "$@"
    '';
    description = "Rebuild the design document when design sources change";
  };

  scripts.lint-prose = {
    exec = ''
      exec vale --config "$DEVENV_ROOT/.vale.ini" \
        "$DEVENV_ROOT/design/main.typ" \
        "$DEVENV_ROOT"/*.md \
        "$DEVENV_ROOT"/ddrs/*.md
    '';
    description = "Check maintained prose with Vale";
  };

  git-hooks.hooks.vale = {
    enable = true;
    package = vale;
    files = "^([^/]+\\.md|ddrs/.*\\.md|design/main\\.typ)$";
    settings.configPath = ".vale.ini";
  };

  enterTest = ''
    "$DEVENV_ROOT/bin/build-design"
    test -s "$DEVENV_ROOT/DESIGN.pdf"
  '';
}
