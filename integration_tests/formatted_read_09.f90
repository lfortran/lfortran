! Formatted READ with the binary B edit descriptor into integer(int8)
! elements, then TRANSFER of the bytes to real(real64) (#11942).
program formatted_read_09
  use iso_fortran_env, only : int8, real64
  implicit none
  character(64) :: s, s2
  integer(int8) :: buffer(8), expected(8)
  real(real64)  :: n
  integer       :: i

  expected = [0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, -16_int8, 63_int8]
  s = '0000000000000000000000000000000000000000000000001111000000111111'
  read(s, '(8B8.8)') buffer
  print '(A,8(1x,I4))', 'buffer bytes:', (buffer(i), i=1,8)
  if (any(buffer /= expected)) error stop
  n = transfer(buffer, n)
  print '(A,ES23.15)', 'as real64   :', n
  if (abs(n - 1.0_real64) > 1.0e-12_real64) error stop

  write(s2, '(8B8.8)') buffer
  if (s2 /= s) error stop
end program formatted_read_09
