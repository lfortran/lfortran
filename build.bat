cmake ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DWITH_LLVM=yes ^
    -DWITH_STACKTRACE=no ^
    -DCMAKE_PREFIX_PATH="%CONDA_PREFIX%" ^
    -DCMAKE_INSTALL_PREFIX=%cd%/inst ^
    .
cmake --build . --config Release -j2 --target install
