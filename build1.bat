cmake ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DWITH_LLVM=yes ^
    -DLFORTRAN_BUILD_ALL=yes ^
    -DWITH_STACKTRACE=no ^
    -DCMAKE_PREFIX_PATH="%CONDA_PREFIX%" ^
    -DCMAKE_INSTALL_PREFIX=%cd%/inst ^
    -DCMAKE_EXPORT_COMPILE_COMMANDS=yes ^
    .
cmake --build . --config Release --target install