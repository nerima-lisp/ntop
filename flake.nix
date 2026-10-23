{
  description = "A terminal system monitor written in Common Lisp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-tty-kit = {
      url = "github:nerima-lisp/cl-tty-kit/v1.6.1";
      flake = false;
    };
    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.6.0";
      flake = false;
    };
    cl-cli = {
      url = "github:nerima-lisp/cl-cli/v1.4.0";
      flake = false;
    };
    cl-regex-kit = {
      url = "github:nerima-lisp/cl-regex-kit/v2.2.0";
      flake = false;
    };
    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/v0.3.1";
      flake = false;
    };
    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/v0.6.1";
      flake = false;
    };
    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/v2.3.0";
      flake = false;
    };
    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/v1.2.0";
      flake = false;
    };
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.2.0";
      flake = false;
    };
    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.2.0";
      flake = false;
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.5.0";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-nix-forge,
      cl-tty-kit,
      cl-codec-kit,
      cl-cli,
      cl-regex-kit,
      cl-host-kit,
      cl-concurrent-kit,
      cl-boundary-kit,
      cl-date-kit,
      cl-parser-kit,
      cl-json-kit,
      cl-weave,
      ...
    }:
    let
      lib = nixpkgs.lib;
      systems = [ "x86_64-linux" ];

      sourceRoot = builtins.path {
        path = ./.;
        name = "ntop-source";
        filter =
          path: type:
          let
            pathName = builtins.baseNameOf (toString path);
          in
          (type == "directory"
            && !(builtins.elem pathName [
              ".git"
              ".jj"
              ".mediator"
              ".serena"
              ".worktrees"
              "build"
              "coverage"
              "result"
            ]))
          || type == "regular";
      };

      meta = {
        description = "A terminal system monitor written in Common Lisp";
        homepage = "https://github.com/nerima-lisp/ntop";
        license = lib.licenses.mit;
        platforms = systems;
        mainProgram = "ntop";
      };

      sibling =
        ctx:
        {
          name,
          source,
          dependencies ? [ ],
        }:
        ctx.cl.lispDerivation {
          pname = name;
          version = ctx.cl.fromAsdSystem "${source}/${name}.asd";
          src = source;
          lispSystem = name;
          lispDependencies = dependencies;
        };

      siblingsFor =
        ctx:
        let
          mkSibling = sibling ctx;
        in
        rec {
          clHostKit = mkSibling {
            name = "cl-host-kit";
            source = cl-host-kit;
          };
          clCodecKit = mkSibling {
            name = "cl-codec-kit";
            source = cl-codec-kit;
          };
          clDateKit = mkSibling {
            name = "cl-date-kit";
            source = cl-date-kit;
          };
          clBoundaryKit = mkSibling {
            name = "cl-boundary-kit";
            source = cl-boundary-kit;
            dependencies = [ clHostKit ];
          };
          clConcurrentKit = mkSibling {
            name = "cl-concurrent-kit";
            source = cl-concurrent-kit;
            dependencies = [
              clBoundaryKit
              clDateKit
            ];
          };
          clParserKit = mkSibling {
            name = "cl-parser-kit";
            source = cl-parser-kit;
          };
          clTtyKit = mkSibling {
            name = "cl-tty-kit";
            source = cl-tty-kit;
            dependencies = [
              clCodecKit
              clConcurrentKit
            ];
          };
          clCli = mkSibling {
            name = "cl-cli";
            source = cl-cli;
            dependencies = [ clHostKit ];
          };
          clRegexKit = mkSibling {
            name = "cl-regex-kit";
            source = cl-regex-kit;
            dependencies = [
              clConcurrentKit
              clParserKit
            ];
          };
          clJsonKit = mkSibling {
            name = "cl-json-kit";
            source = cl-json-kit;
          };
          clWeave = mkSibling {
            name = "cl-weave";
            source = cl-weave;
          };
        };
    in
    cl-nix-forge.lib.${builtins.head systems}.mkPackageFlake {
      inherit
        self
        systems
        nixpkgs
        meta
        ;

      pname = "ntop";
      asd = ./ntop.asd;
      root = ./.;
      src = sourceRoot;
      timeoutSeconds = 1800;
      executable = {
        installSource = false;
      };

      lispDependencies =
        ctx:
        with siblingsFor ctx;
        [
          clTtyKit
          clCli
          clRegexKit
          clHostKit
          clConcurrentKit
          clDateKit
          clParserKit
        ];

      lispCheckDependencies =
        ctx:
        with siblingsFor ctx;
        [ clWeave ];

      extraOutputs =
        ctx:
        let
          pkgs = nixpkgs.legacyPackages.${ctx.system};
        in
        {
          checks.build = ctx.executable;
          checks.coverage = ctx.cl.mkScriptCheck {
            drv = ctx.package;
            entryPoint = "scripts/coverage.lisp";
            timeoutSeconds = 1800;
          };
          checks.dependency-depth = ctx.cl.mkScriptCheck {
            drv = ctx.package;
            entryPoint = "scripts/dependency-depth.lisp";
            timeoutSeconds = 120;
          };
          checks.local = pkgs.runCommand "ntop-local-check" {
            nativeBuildInputs = [
              pkgs.coreutils
              pkgs.python3
              pkgs.sbcl
            ];
          } ''
            cd ${sourceRoot}
            timeout --signal=TERM --kill-after=15s 300s \
              python3 scripts/check-local.py --root ${sourceRoot} --sbcl ${pkgs.sbcl}/bin/sbcl
            touch "$out"
          '';
        };
    };
}
