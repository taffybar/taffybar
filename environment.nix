_: pkgs:
let
  addGObjectIntrospection = hpackage: pkgs.haskell.lib.overrideCabal hpackage (current: {
    libraryPkgconfigDepends =
      current.libraryPkgconfigDepends ++ [ pkgs.gobject-introspection ];
  });
in {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      broadcast-chan = pkgs.haskell.lib.overrideCabal super.broadcast-chan (_: {
        version = "0.2.0.2";
        sha256 = "12ax37y9i3cs8wifz01lpq0awm9c235l5xkybf13ywvyk5svb0jv";
        revision = null;
        editedCabalFile = null;
        broken = false;
      });
      gi-pango = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-pango";
        ver = "1.0.21";
        sha256 = "04wnczdd9aifai4p30lmqlhrpx0rz1454sw2xglm1k5z0ha8qcgn";
      } {inherit (pkgs) cairo; inherit (pkgs) pango;});
      gi-atk = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-atk";
        ver = "2.0.20";
        sha256 = "0yzhbcpsbrs8j3rb8rln4vbiwv9lpbwb61r164bc3rpa268ipbyw";
      } { inherit (pkgs) atk; });
      gi-gio = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-gio";
        ver = "2.0.24";
        sha256 = "0vxa3jzyjx7x2p3mx2p0s2jc35dzxpblqz84ad193m3bqcaf1xs7";
      } {inherit (pkgs) glib;});
      gi-gdkpixbuf = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-gdkpixbuf";
        ver = "2.0.22";
        sha256 = "01ghzx4xh2f9qzyvv5a9si6wycxdcfkkp3258b12dvilj97p35r5";
      } {inherit (pkgs) gdk_pixbuf;});
      gi-gdk = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-gdk";
        ver = "3.0.21";
        sha256 = "15dkzfnryvvw8vh1g4is58ahpp60qz5r9va1rw97774zgcx91kd9";
      } {inherit (pkgs) gtk3;});
      gi-gtk = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-gtk";
        ver = "3.0.31";
        sha256 = "15a3l98ai5qfslbyir8svknkwwzq61l0l0njb57k2dq7a84siava";
      } {inherit (pkgs) gtk3;});
      gi-gtk-hs = self.callHackageDirect {
        pkg = "gi-gtk-hs";
        ver = "0.3.7.0";
        sha256 = "0h5959ayjvipj54z0f350bz23fic90xw9z06xw4wcvxvwkrsi2br";
      } { };
      gi-gdkx11 = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-gdkx11";
        ver = "3.0.8";
        sha256 = "0fymsiz1087hwj3aljc53x301m0dasgnpwdhsr907vw7vdmw2pgq";
      } {inherit (pkgs) gtk3;});
      gi-dbusmenu = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-dbusmenu";
        ver = "0.4.6";
        sha256 = "0s03c5kikbbypcm5i0rjklzkihm921bdip68l9ymmigrk8i9wynw";
      } {inherit (pkgs) libdbusmenu;});
      gi-dbusmenugtk3 = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-dbusmenugtk3";
        ver = "0.4.7";
        sha256 = "0g3wnn25631dxsb8wa4l3wbir30ryq29qcr5lv2nbd4k04fmslzc";
      } {inherit (pkgs) gtk3; inherit (pkgs) libdbusmenu-gtk3;});
      gi-gobject = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-gobject";
        ver = "2.0.21";
        sha256 = "11s1gj4p2w64qx18kj09f4mb3nr7lqfafxw5w2pk1wb8d0vsg47i";
      } { inherit (pkgs) glib; });
      gi-cairo = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-cairo";
        ver = "1.0.22";
        sha256 = "0j1ndjpdwpi0zkavrldx7pkwijjnf4r6945kdpfsjkqayy8v94h9";
      } { inherit (pkgs) cairo; });
      gi-xlib = addGObjectIntrospection super.gi-xlib;
      gi-glib = addGObjectIntrospection (self.callHackageDirect {
        pkg = "gi-glib";
        ver = "2.0.22";
        sha256 = "18hszmhmdasrymia8jimyvdclp9qcnhp2rajakmci27nbrilngb1";
      } { inherit (pkgs) glib; });
      gi-cairo-render = pkgs.haskell.lib.overrideCabal (self.callHackageDirect {
        pkg = "gi-cairo-render";
        ver = "0.0.1";
        sha256 = "05vcvmdrz8pad9i64b4vdkjh3y9kahp544vxyvris1w7y5ryc7ib";
      } { inherit (pkgs) cairo; })
      (_: {
        revision = "1";
        editedCabalFile = "10lpmb8js19zfgnph31yz4nzyv7kbqvq1lx07w12q702khqcqb7z";
      });
      haskell-gi-base = self.callHackageDirect {
        pkg = "haskell-gi-base";
        ver = "0.22.2";
        sha256 = "0bdmhvq2bw5mf135wf94hdc3xidjzyr9y3bmvx30pgyv6hflldki";
      }
      { inherit (pkgs) glib; };
      haskell-gi = self.callHackageDirect {
        pkg = "haskell-gi";
        ver = "0.22.6";
        sha256 = "0pizgz6846i19fi0avsqlqxal1f09ihpd9qlyyhci4j3fvgvrsl0";
      }
      { inherit (pkgs) glib gobject-introspection; };
    });
  });
}
