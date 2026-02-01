! Test for https://github.com/lfortran/lfortran/issues/3938
! bit_size used in character length specification
program intrinsics_411
  implicit none
  character(bit_size(666)):: foo
  if (bit_size(666) /= 32) error stop
  if (len(foo) /= 32) error stop
  print *, bit_size(666), len(foo)
end program intrinsics_411
