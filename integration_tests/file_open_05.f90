program file_open_5
  use iso_fortran_env, only: output_unit

  open(output_unit, encoding="UTF-8")
  print *, "Hello"
end program file_open_5
