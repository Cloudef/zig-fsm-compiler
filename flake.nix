{
  description = "Zig project flake";

  inputs = {
    zig2nix.url = "github:Cloudef/zig2nix";
  };

  outputs = { zig2nix, ... }: let
    flake-utils = zig2nix.inputs.flake-utils;
  in (flake-utils.lib.eachDefaultSystem (system: let
      # Zig flake helper
      # Check the flake.nix in zig2nix project for more options:
      # <https://github.com/Cloudef/zig2nix/blob/master/flake.nix>
      env = zig2nix.outputs.zig-env.${system} { zig = zig2nix.outputs.packages.${system}.zig.master.bin; };
      system-triple = env.lib.zigTripleFromString system;
    in with builtins; with env.lib; with env.pkgs.lib; rec {
      # nix build .#target.{nix-target}
      # e.g. nix build .#target.x86_64-linux
      packages.target = genAttrs allTargetTriples (target: env.packageForTarget target ({
        src = ./.;

        # Smaller binaries and avoids shipping glibc.
        zigPreferMusl = true;

        # This disables LD_LIBRARY_PATH mangling, binary patching etc...
        # The package won't be usable inside nix.
        zigDisableWrap = true;
      }));

      # nix build .
      packages.default = packages.target.${system-triple}.override {
        # Prefer nix friendly settings.
        zigPreferMusl = false;
        zigDisableWrap = false;
      };

      # For bundling with nix bundle for running outside of nix
      # example: https://github.com/ralismark/nix-appimage
      apps.bundle.target = genAttrs allTargetTriples (target: let
        pkg = packages.target.${target};
      in {
        type = "app";
        program = "${pkg}/bin/zig-fsm-compiler";
      });

      # default bundle
      apps.bundle.default = apps.bundle.target.${system-triple};

      # nix run .
      apps.default = env.app [] "zig build run -- \"$@\"";

      # nix run .#build
      apps.build = env.app [] "zig build \"$@\"";

      # nix run .#test
      apps.test = env.app [] "zig build test -- \"$@\"";

      # nix run .#docs
      apps.docs = env.app [] "zig build docs -- \"$@\"";

      # nix run .#zon2json
      apps.zon2json = env.app [env.zon2json] "zon2json \"$@\"";

      # nix run .#zon2json-lock
      apps.zon2json-lock = env.app [env.zon2json-lock] "zon2json-lock \"$@\"";

      # nix run .#zon2nix
      apps.zon2nix = env.app [env.zon2nix] "zon2nix \"$@\"";

      # nix develop
      devShells.default = env.shell;

      # nix run .#readme
      apps.readme = let
        project = "zig-fsm-compiler";
      in with env.pkgs; env.app [graphviz] (builtins.replaceStrings ["`"] ["\\`"] ''
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
    }));
}
