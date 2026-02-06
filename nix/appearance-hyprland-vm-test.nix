{ pkgs
, taffybarPackage
, cssFile
, goldenFile ? null
, compare ? false
}:

let
  user = "taffy";
  uid = 1000;
  # The VM test only needs the executables; running the full Hspec suite here
  # would duplicate `checks.tests` and adds flake-prone X11 screenshot
  # comparisons.
  taffybarPkg = pkgs.haskell.lib.dontCheck taffybarPackage;
in
pkgs.testers.nixosTest (
  { lib, ... }:
  {
    name = "taffybar-appearance-hyprland" + lib.optionalString (!compare) "-snapshot";

    # make-test-python's type checker doesn't understand some of the Python
    # helper method return types well enough to accept this script.
    skipTypeCheck = true;

    nodes.machine =
      { config, pkgs, lib, ... }:
      {
        users.users.${user} = {
          isNormalUser = true;
          uid = uid;
          group = user;
          extraGroups = [ "wheel" "video" "input" ];
        };
        users.groups.${user} = { };

        # Autologin so we can start Hyprland on tty1 (this creates a logind
        # session, which the compositor needs to open the DRM device).
        services.getty.autologinUser = user;

        programs.hyprland.enable = true;

        environment.systemPackages = with pkgs; [
          taffybarPkg
          grim
          imagemagick
          jq
        ];

        environment.etc."taffybar/appearance-test.css".source = cssFile;
        environment.etc."taffybar/appearance-hyprland-bar.png" = lib.mkIf compare {
          source = goldenFile;
        };

        # Keep fonts predictable.
        fonts.packages = [ pkgs.dejavu_fonts ];

        # Best-effort: allow software rendering inside the VM.
        environment.variables = {
          WLR_RENDERER_ALLOW_SOFTWARE = "1";
          WLR_NO_HARDWARE_CURSORS = "1";
        };

        # Automatically configure and start Hyprland when logging in on tty1.
        programs.bash.loginShellInit = ''
          if [ "$(tty)" = "/dev/tty1" ]; then
            set -euo pipefail
            mkdir -p ~/.config/hypr
            cat > ~/.config/hypr/hyprland.conf <<'EOF'
          monitor=,1024x768@60,0x0,1

          misc {
            disable_hyprland_logo = true
            disable_splash_rendering = true
          }

          animations {
            enabled = false
          }

          decoration {
            rounding = 0
            blur { enabled = false }
          }

          general {
            gaps_in = 0
            gaps_out = 0
            border_size = 0
          }
          EOF

            ${lib.getExe config.programs.hyprland.package} \
              --config ~/.config/hypr/hyprland.conf \
              2>/tmp/hyprland.log \
              && touch /tmp/hyprland-exit-ok
          fi
        '';

        # Need a DRM-capable virtual GPU (the default "-vga std" is often not
        # sufficient for wlroots compositors).
        virtualisation.qemu.options = [ "-vga none -device virtio-gpu-pci" ];
        virtualisation.resolution = {
          x = 1024;
          y = 768;
        };
      };

    testScript = ''
      import shlex

      q = shlex.quote


      def run_as(user, cmd: str):
          return machine.execute(f"su - {user} -c {q(cmd)}")


      start_all()
      machine.wait_for_unit("multi-user.target")

      # Wait for Hyprland session startup.
      machine.wait_until_succeeds("pgrep -u ${user} Hyprland")
      machine.wait_until_succeeds("test -d /run/user/${toString uid}")
      machine.wait_until_succeeds(
          "ls /run/user/${toString uid} | grep -E '^wayland-[0-9]+$' | head -n1"
      )

      wayland = machine.succeed(
          "ls -1 /run/user/${toString uid} | grep -E '^wayland-[0-9]+$' | head -n1"
      ).strip()

      machine.wait_until_succeeds(
          "test -d /run/user/${toString uid}/hypr && ls -1 /run/user/${toString uid}/hypr | head -n1"
      )
      sig = machine.succeed("ls -1 /run/user/${toString uid}/hypr | head -n1").strip()
      machine.wait_until_succeeds(f"test -S /run/user/${toString uid}/hypr/{sig}/.socket.sock")

      env = " ".join(
          [
              "XDG_RUNTIME_DIR=/run/user/${toString uid}",
              f"WAYLAND_DISPLAY={wayland}",
              f"HYPRLAND_INSTANCE_SIGNATURE={sig}",
              "GDK_BACKEND=wayland",
              "XDG_SESSION_TYPE=wayland",
              "GDK_SCALE=1",
              "GDK_DPI_SCALE=1",
              "GTK_CSD=0",
              "GTK_THEME=Adwaita",
              "NO_AT_BRIDGE=1",
              "GSETTINGS_BACKEND=memory",
          ]
      )

      out_png = "/tmp/appearance-hyprland-bar.png"
      css = "/etc/taffybar/appearance-test.css"

      # Sanity check: ensure grim can capture from this Wayland session.
      grim_test = "/tmp/grim-test.png"
      run_as(
          "${user}",
          f"env {env} grim -g '0,0 10x10' {grim_test}",
      )
      machine.succeed(f"test -s {grim_test}")

      status, out = run_as(
          "${user}",
          f"env {env} taffybar-appearance-snap-hyprland --out {out_png} --css {css}",
      )
      if out.strip():
          machine.log("taffybar-appearance-snap-hyprland output:\n" + out)

      # Always copy artifacts to $out for local inspection (useful with
      # --keep-failed).
      if machine.execute("test -f /tmp/hyprland.log")[0] == 0:
          machine.copy_from_vm("/tmp/hyprland.log", "artifacts")
      if machine.execute(f"test -f {out_png}")[0] == 0:
          machine.copy_from_vm(out_png, "artifacts")

      if status != 0:
          # Extra debugging: capture the full output image so we can see what is
          # actually being rendered inside the compositor.
          debug_full = "/tmp/appearance-hyprland-full.png"
          run_as(
              "${user}",
              f"env {env} grim {debug_full}",
          )
          if machine.execute(f"test -s {debug_full}")[0] == 0:
              machine.copy_from_vm(debug_full, "artifacts")

          if machine.execute("test -f /tmp/hyprland.log")[0] == 0:
              machine.log(
                  "hyprland.log (tail):\n"
                  + machine.succeed("tail -n 200 /tmp/hyprland.log")
              )

          hypr_clients_status, hypr_clients = run_as(
              "${user}",
              f"env {env} hyprctl -j clients",
          )
          if hypr_clients.strip():
              machine.log("hyprctl clients:\n" + hypr_clients)
          elif hypr_clients_status != 0:
              machine.log("hyprctl clients failed")

          hypr_ws_status, hypr_ws = run_as(
              "${user}",
              f"env {env} hyprctl -j workspaces",
          )
          if hypr_ws.strip():
              machine.log("hyprctl workspaces:\n" + hypr_ws)
          elif hypr_ws_status != 0:
              machine.log("hyprctl workspaces failed")

          hypr_mon_status, hypr_mon = run_as(
              "${user}",
              f"env {env} hyprctl -j monitors",
          )
          if hypr_mon.strip():
              machine.log("hyprctl monitors:\n" + hypr_mon)
          elif hypr_mon_status != 0:
              machine.log("hyprctl monitors failed")

          raise Exception(f"taffybar-appearance-snap-hyprland failed with exit code {status}")

      machine.succeed(f"test -s {out_png}")

      if ${if compare then "True" else "False"}:
          golden = "/etc/taffybar/appearance-hyprland-bar.png"
          # ImageMagick compare: 0 = identical, 1 = different, 2 = error.
          status, out = machine.execute(
              f"compare -metric AE {golden} {out_png} null: 2>&1"
          )
          if status != 0:
              machine.log("Appearance mismatch (ImageMagick compare output): " + out.strip())
              # Copy a dereferenced golden (avoid symlink weirdness) and a diff
              # image for debugging.
              golden_copy = "/tmp/appearance-hyprland-bar.golden.png"
              diff_copy = "/tmp/appearance-hyprland-bar.diff.png"
              machine.succeed(f"cp -L {golden} {golden_copy}")
              machine.copy_from_vm(golden_copy, "artifacts")

              machine.execute(f"compare {golden} {out_png} {diff_copy}")
              if machine.execute(f"test -s {diff_copy}")[0] == 0:
                  machine.copy_from_vm(diff_copy, "artifacts")
              raise Exception("Hyprland appearance golden mismatch")
    '';
  }
)
