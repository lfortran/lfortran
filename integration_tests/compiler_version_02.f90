program compiler_version_02
  use, intrinsic :: iso_fortran_env, only: compiler_version
  print*, compiler_version()
end program
