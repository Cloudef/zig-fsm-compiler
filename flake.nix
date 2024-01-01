{
  description = "zig-fsm-compiler flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    zig-stdenv.url = "github:Cloudef/nix-zig-stdenv";
  };

  outputs = { flake-utils, nixpkgs, zig-stdenv, ... }:
  (flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.outputs.legacyPackages."${system}";
      zig = zig-stdenv.versions.${system}.master;
      app = deps: script: {
        type = "app";
        program = toString (pkgs.writeShellApplication {
          name = "app";
          runtimeInputs = [ zig ] ++ deps;
          text = ''
            # shellcheck disable=SC2059
            error() { printf -- "error: $1" "''${@:1}" 1>&2; exit 1; }
            [[ -f ./flake.nix ]] || error 'Run this from the project root'
            export ZIG_BTRFS_WORKAROUND=1
            ${script}
            '';
        }) + "/bin/app";
      };
    in {
      # nix run
      apps.default = app [] "zig build run -- \"$@\"";

      # nix run .#test
      apps.test = app [] "zig build test";

      # nix run .#docs
      apps.docs = app [] "zig build docs";

      # nix run .#version
      apps.version = app [] "zig version";

      # nix run .#readme
      apps.readme = let
        project = "zig-fsm-compiler";
      in with pkgs; app [graphviz] (builtins.replaceStrings ["`"] ["\\`"] ''
      function graph() {
        mkdir -p images
        (printf -- '%%%%{machine %s;main:=%s;}%%%%' "$2" "$1" | zig build run -- -Vp | dot -Tpng -oimages/fsm-"$2".png)
        printf -- '> ![%s machine](%s)\n\n' "$2" "./images/fsm-$2.png"
      }

      rm -f images/fsm-*.png

      cat <<EOF
      # ${project}

      Ragel compatible FSM compiler for Zig

      > [!CAUTION]
      > This project is still in WIP stage.
      > You won't be able to use it yet.
      >
      > Once the ragel language scanner itself can be compiled, it should be usable.

      ---

      [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

      Project is tested on zig version $(zig version)

      ## How to use

      ${project} can be used either as a standalone compiler or directly from your zig program as a module.

      ### Standalone usage

      ```
      $(zig build run -- --help)
      ```

      ### Module usage

      First add ${project} to your project

      `build.zig.zon`
      ```zig
      .fsm_compiler = .{
        .url = "https://github.com/Cloudef/zig-fsm-compiler/archive/{COMMIT}.tar.gz",
        .hash = "{HASH}",
      },
      ```

      `build.zig`
      ```zig
      const fsm_compiler = b.dependency("fsm_compiler", .{}).module("fsm-compiler");
      exe.addModule("fsm-compiler", fsm_compiler);
      ```

      ---

      `main.zig`
      ```zig
      const fsm_compiler = @import("fsm-compiler");

      fn main() !void {
        // TODO
      }
      ```

      ### Comptime usage

      It is not possible to run the compiler itself comptime, but it is possible to use the generated output comptime.
      ${project} provides "run artifact" that can be used to generate machines from your `build.zig`.

      > [!NOTE]
      > Running compiler comptime may be possible once https://github.com/ziglang/zig/issues/14931 is fixed.

      `build.zig`
      ```zig
      const std = @import("std");
      const fsm_compiler = @import("fsm-compiler");

      pub fn build(b: *std.Build) void {
        // TODO
      }
      ```

      ## Ragel

      [Ragel](https://www.colm.net/open-source/ragel) is a state machine compiler by Adrian Thurston.
      It is highly recommended to read this [PDF](https://www.colm.net/files/ragel/ragel-guide-6.10.pdf) to learn the
      Ragel language.

      ${project} is written by studying the above PDF, it contains no code from the actual Ragel project aside from
      the modified `rlscan.rl` to bootstrap the initial version of the Ragel language parser.

      ### Differences from Ragel

      While ${project} aims to be compatible with the Ragel language there are still differences.

      * Only zig target is supported for code generation.
        - *Other languages may be supported in future, but it is not a priority.*
      * Actions with code only works if using code generation.
        - *It is possible to bind functions to actions to get past this limitation.*
      * getkey, access, variable, write statements are ignored and hidden variables do not exist.
        - *Instead the compiler generates a optimized machine and runner with an entrypoint for executing the machine.*
        - *This means ${project} is not compatible with the output of Ragel.*
      * alphtype statement is ignored.
        - *Instead the compiler always uses the smallest types possible for the optimized machine.*

      ### Examples

      $(graph '/ab*[c]d*[123]/' "regex")
      $(graph '/[a-z]*[!]/*' "kleene")
      $(graph '[a-z]+' "repetition")
      EOF
      '');

      # nix develop
      devShells.default = pkgs.mkShell {
        buildInputs = [ zig ];
        shellHook = "export ZIG_BTRFS_WORKAROUND=1";
      };
    }));
}
