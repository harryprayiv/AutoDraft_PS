{
  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      ps-tools.follows = "purs-nix/ps-tools";
      purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
      utils.url = "github:numtide/flake-utils";
    };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix = inputs.purs-nix { inherit system; };

          ps =
            purs-nix.purs
              {
                dependencies =
                  with purs-nix.ps-pkgs;
                  [
                    console
                    effect
                    halogen
                    prelude
                  ];

                dir = ./.;
              };
          ps-command = ps.command { };
          concurrent = pkgs.writeShellApplication {
            name = "concurrent";
            runtimeInputs = with pkgs; [
              concurrently
            ];
            text = ''
              concurrently\
                --color "auto"\
                --prefix "[{command}]"\
                --handle-input\
                --restart-tries 10\
                "$@"
            '';
          };
          purs-watch = pkgs.writeShellApplication {
            name = "purs-watch";
            runtimeInputs = with pkgs; [ entr ps-command ];
            text = ''find {src,test} | entr -s "purs-nix $*"'';
          };
          webpack = pkgs.writeShellApplication {
            name = "webpack";
            runtimeInputs = with pkgs; [ nodejs ];
            text = ''npx webpack "$@"'';
          };
          serve = pkgs.writeShellApplication {
            name = "serve";
            runtimeInputs = with pkgs; [ webpack ];
            text = ''BROWSER_RUNTIME=1 webpack serve --progress --open "$@"'';
          };
          dev = pkgs.writeShellApplication {
            name = "dev";
            runtimeInputs = with pkgs; [
              concurrent
              purs-watch
              serve
            ];
            text = ''
              concurrent \
                "purs-watch compile"\
                serve\
                "runtime up"
            '';
          };
          bundle = pkgs.writeShellApplication {
            name = "bundle";
            runtimeInputs = with pkgs; [ webpack ];
            text = ''BROWSER_RUNTIME=1 webpack --mode=production "$@"'';
          };
          vite = pkgs.writeShellApplication {
            name = "vite";
            runtimeInputs = with pkgs; [ nodejs ];
            text = "npx vite --open";
          };
          tests = pkgs.writeShellApplication {
            name = "tests";
            text = ''purs-watch test "$@"'';
            runtimeInputs = [ purs-watch ];
          };
          # checks = pkgs.runCommand "checks"
          #   {
          #     buildInputs = testRuntime;
          #   } ''${ps.test.run { }}; touch $out'';
        in
        {
          packages.default = ps.modules.Main.bundle { };

          # checks.default = checks;

          devShells.default =
            pkgs.mkShell
              {
                packages =
                  with pkgs;
                  [
                    nodePackages.webpack-cli
                    ps-tools.for-0_15.purescript-language-server
                    ps-command
                    purs-nix.purescript
                    purs-watch
                    vite
                    dev
                    bundle
                  ];
              };
        }
      );
}
