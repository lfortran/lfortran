program derived_types_same_name_01
  use dtsn01_small_m
  use dtsn01_big_m
  implicit none
  call run_small()
  call run_big()
  print *, "ok"
end program
