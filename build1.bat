cmake ^
    -GNinja ^
    -DCMAKE_C_COMPILER=cl.exe ^
    -DCMAKE_CXX_COMPILER=cl.exe ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DWITH_LLVM=yes ^
    -DLFORTRAN_BUILD_ALL=yes ^
    -DWITH_STACKTRACE=no ^
    -DCMAKE_PREFIX_PATH="%CONDA_PREFIX%" ^
    -DCMAKE_INSTALL_PREFIX=%cd%/inst ^
    .
cmake --build . --config Release -j2 --target install
