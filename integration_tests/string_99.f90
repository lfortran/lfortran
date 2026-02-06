! Test for https://github.com/lfortran/lfortran/issues/3938
! character length from bit_size() intrinsic, len() correctness
! Exact MRE from issue body
program bitsize2
  implicit none
  character(bit_size(666)):: foo
  print *, bit_size(666), len(foo)
  if (bit_size(666) /= 32) error stop
  if (len(foo) /= 32) error stop
end program bitsize2
