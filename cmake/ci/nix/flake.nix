{
  description = "LFortran devShell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }: 
    let
      # TODO: add and test more platforms
      systems = [ "x86_64-linux" ];
      forEachSystem = f: builtins.listToAttrs (map (system: {
        name = system;
        value = {
          default = f system false;
          clangOnly = f system true;
        };
      }) systems);
    in
    {
      devShells = forEachSystem (system: clangOnly:
        let
          pkgs = import nixpkgs { inherit system; };

          llvmVersion = "20";
          pythonVersion = "313";

          llvmPkgs = pkgs.buildPackages."llvmPackages_${llvmVersion}";
          myStdenv = if clangOnly then llvmPkgs.stdenv else pkgs.gcc15Stdenv;
          myBinutils = if clangOnly then llvmPkgs.bintools else pkgs.binutils;
          mkShellNewEnv = pkgs.mkShell.override { stdenv = myStdenv; };

          # We need version 3.4 specifically, see:
          # https://lfortran.zulipchat.com/#narrow/channel/197339-General/topic/Building.20Lfortran/near/479058294
          # TODO: get bison building under clang (use myStdenv) (if at all possible)
          bison_3_4 = pkgs.gcc15Stdenv.mkDerivation rec {
            pname = "bison";
            version = "3.4.2";

            src = pkgs.fetchurl {
              url = "mirror://gnu/bison/${pname}-${version}.tar.xz";
              sha256 = "sha256-J9BVNGmXNdxp6GrdW4CNbLNZAK0/1j+oLj62RDNqv6A=";
            };

            nativeBuildInputs = with pkgs; [ m4 flex perl ];
            propagatedBuildInputs = with pkgs; [ m4 ];
            configureFlags = [ "--disable-dependency-tracking" ];

            doCheck = false;
            doInstallCheck = true;
          };

          # Probably too niche to be in nixpkgs
          jupyter_kernel_test = pkgs."python${pythonVersion}Packages".buildPythonPackage rec {
            pname = "jupyter_kernel_test";
            version = "0.7.0";
            format = "pyproject";

            src = pkgs.fetchPypi {
              inherit pname version;
              sha256 = "sha256-B4tv5/dw3RZPlUm916NVZjIlo/+bD3V1rVRtJyOexgk=";
            };

            nativeBuildInputs = with pkgs."python${pythonVersion}Packages"; [
              hatchling
              jsonschema
              jupyter-client
            ];
          };

          # nixpkgs version is 1.3.0, i.e. too old for us
          # this is largely copied from nixpkgs:
          xeus_zmq_3_0 = llvmPkgs.stdenv.mkDerivation rec {
            pname = "xeus-zmq";
            version = "3.0.0";

            src = pkgs.fetchFromGitHub {
              owner = "jupyter-xeus";
              repo = "xeus-zmq";
              rev = "${version}";
              hash = "sha256-J9an+D1FLw99uJPCuux4YNRFcBMh24N3+GDoS/G/U28=";
            };

            nativeBuildInputs = with pkgs; [ cmake ];

            buildInputs = with pkgs; [
              cppzmq
              libuuid
              openssl
              xeus
              xtl
              zeromq
            ];

            propagatedBuildInputs = with pkgs; [ nlohmann_json ];
          };
        in mkShellNewEnv ({
          buildInputs = with pkgs; [
            pkgs."llvm_${llvmVersion}"
            pkgs."lld_${llvmVersion}"
            myBinutils

            (pkgs."python${pythonVersion}".withPackages (python-pkgs: [
              python-pkgs.pytest
              python-pkgs.toml
              python-pkgs.numpy
              python-pkgs.jupyter
              jupyter_kernel_test
            ]))
            xonsh

            cmake
            ninja
            ccache
            kokkos

            git
            re2c
            bison_3_4

            pkgs.pkgsStatic.zlib
            pkgs.pkgsStatic.zstd
            xz
            libbfd
            libunwind
            rapidjson
            libxml2
            libuuid
            openssl

            pandoc
            xeus
            xeus_zmq_3_0
            nlohmann_json

            clang  # gcc env fails tests without clang available
            bashInteractive
            which
            gfortran
            valgrind
            gdb
            fmt
          ];
          shellHook = ''
            set -x
            echo "LFortran devShell ready, software versions:"
            c++ --version
            bison --version
            llvm-config --version
            re2c --version
            ${
              if clangOnly then "ld.lld --version" else "ld --version"
            }
            set +x
          '';
        })
      );
    };
}

