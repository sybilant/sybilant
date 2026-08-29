{ pkgs, lib, ... }:

let
  # Vale's native Typst support (linting docs/**/*.typ) needs Vale >=
  # 3.18.0. nixpkgs tops out at 3.17.1 as of this writing, so we pull the
  # upstream release binary instead of the packaged one.
  vale = pkgs.stdenv.mkDerivation rec {
    pname = "vale";
    version = "3.19.0";

    src = pkgs.fetchurl {
      url = "https://github.com/errata-ai/vale/releases/download/v${version}/vale_${version}_Linux_64-bit.tar.gz";
      sha256 = "sha256-yPnWyAVUQrx+nBIbJJjm8OP7Zw9GZebuV38Yl/dmXPY=";
    };

    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    buildInputs = [ pkgs.stdenv.cc.cc.lib ];

    sourceRoot = ".";

    installPhase = ''
      runHook preInstall
      install -Dm755 vale $out/bin/vale
      runHook postInstall
    '';

    meta = with lib; {
      description = "A syntax-aware linter for prose";
      homepage = "https://vale.sh";
      license = licenses.mit;
      platforms = [ "x86_64-linux" ];
    };
  };

  # typst2vast is Vale's companion parser for Typst (reads Typst syntax
  # without evaluating it, so prose can be linted without the doc needing
  # to compile). It isn't in nixpkgs, so it's packaged here from crates.io.
  typst2vast = pkgs.rustPlatform.buildRustPackage rec {
    pname = "typst2vast";
    version = "0.1.0";

    src = pkgs.fetchCrate {
      inherit pname version;
      hash = "sha256-TQD70ckrn9trHCtBNiuLzb280Nq36kRDJthnKwOikWw=";
    };

    cargoHash = "sha256-Uodxp2ejlYtL3W5n63XoiEp5gK16w38OhoCGPR3vCLY=";

    meta = with lib; {
      description = "CLI to convert Typst to HTML for Vale, parsing without evaluating";
      homepage = "https://github.com/jdkato/typst2vast";
      license = licenses.mit;
    };
  };
in
{
  # https://devenv.sh/packages/
  packages = [
    vale
    typst2vast
    pkgs.typst
    pkgs.evince
    pkgs.inotify-tools
    pkgs.poppler-utils
    pkgs.ripgrep
    pkgs.nasm
    pkgs.binutils
    pkgs.gnumake
  ];

  # Typst (the PDF pipeline's render engine, see scripts.build-manual
  # below) ships no bundled fonts. Point it at a Nix-packaged font, via
  # Typst's own TYPST_FONT_PATHS, so PDF builds don't depend on whatever
  # fonts happen to be installed on the system.
  env.TYPST_FONT_PATHS = "${pkgs.libertinus}/share/fonts";

  # `build-manual` compiles docs/sybilant-manual.typ (which #includes the
  # DDRs) into docs/sybilant-manual.pdf via Typst. See bin/build-manual
  # for the actual build.
  scripts.build-manual.exec = ''
    exec "$DEVENV_ROOT/bin/build-manual" "$@"
  '';

  # `watch-manual` rebuilds the manual on every source change and keeps
  # it open in evince. See bin/watch-manual.
  scripts.watch-manual.exec = ''
    exec "$DEVENV_ROOT/bin/watch-manual" "$@"
  '';

  # https://devenv.sh/git-hooks/
  #
  # Lints prose in Markdown and Typst files with Vale, using the Google
  # developer documentation style guide. Installed as a git pre-commit
  # hook the first time you enter this devenv shell, and runs against
  # staged files.
  #
  # All Vale configuration -- including which Google style checks are
  # enabled -- lives under .config/vale/vale.ini.
  git-hooks.hooks.vale = {
    enable = true;
    files = "\\.(md|typ)$";
    settings.configPath = ".config/vale/vale.ini";
    # git-hooks.nix's built-in "vale" hook type otherwise hardcodes its own
    # nixpkgs `vale` (3.17.1, predates Typst support) -- point it at the
    # 3.19.0 build above instead, so the hook lints .typ the same way
    # `vale` does inside `devenv shell`. `package` alone isn't enough:
    # linting .typ also needs typst2vast on PATH, so the entry is
    # overridden outright rather than relying on the built-in "vale" hook
    # type's generated command. The trailing `--` becomes bash's $0, so
    # the filenames git-hooks.nix appends land in "$@" starting at $1.
    package = vale;
    entry = ''
      bash -c 'PATH="${typst2vast}/bin:$PATH" exec "${vale}/bin/vale" --config .config/vale/vale.ini "$@"' --
    '';
  };

  # Rebuilds the manual and stages the result, so a fresh
  # docs/sybilant-manual.pdf always rides along with the .typ change that
  # produced it. Doesn't rely on an active `devenv shell`: PATH and
  # TYPST_FONT_PATHS are set explicitly to the same Nix-packaged typst
  # and font used everywhere else in this file.
  #
  # Scoped to the manual's only input -- DDRs aren't included in the PDF
  # (see docs/ddrs/README.md) -- so it doesn't fire on an unrelated
  # staged .typ change.
  git-hooks.hooks.build-manual = {
    enable = true;
    name = "build-manual";
    files = "^docs/sybilant-manual\\.typ$";
    pass_filenames = false;
    language = "system";
    entry = ''
      bash -c 'PATH="${pkgs.typst}/bin:$PATH" TYPST_FONT_PATHS="${pkgs.libertinus}/share/fonts" bin/build-manual && git add docs/sybilant-manual.pdf'
    '';
  };

  # See full reference at https://devenv.sh/reference/options/
}
