program read_61
  use, intrinsic :: iso_fortran_env, only: real32, real64, int32
  implicit none

  character(len=:), allocatable :: s
  real(real32) :: x, y
  real(real64) :: d
  integer(int32) :: got_bits, exp_bits

  ! Legacy exponent shorthand with minus sign.
  s = '1.-3'
  read(s, *) x
  if (abs(x - 1.0e-3_real32) > 1.0e-7_real32) error stop '1.-3 parse failed'

  ! Legacy exponent shorthand with plus sign.
  s = '1.+3'
  read(s, *) x
  if (abs(x - 1.0e3_real32) > 1.0e-3_real32) error stop '1.+3 parse failed'

  ! Multiple values to verify position tracking for internal list-directed reads.
  s = '2.-2 3.+1'
  read(s, *) x, y
  if (abs(x - 2.0e-2_real32) > 1.0e-7_real32) error stop '2.-2 parse failed'
  if (abs(y - 3.0e1_real32) > 1.0e-4_real32) error stop '3.+1 parse failed'

  ! Standard E exponent remains supported.
  s = '4.5E-2'
  read(s, *) x
  if (abs(x - 4.5e-2_real32) > 1.0e-7_real32) error stop 'E exponent parse failed'

  ! Standard D exponent remains supported.
  s = '6.25D+1'
  read(s, *) d
  if (abs(d - 6.25d1) > 1.0d-12) error stop 'D exponent parse failed'

  ! Long token regression: exponent must not be dropped by internal list-directed read.
  s = '0.140129846432481707092372958328991613128026194187651577' // &
      '175706828388979108268586060148663818836212158203125E-44'
  read(s, *) x

  got_bits = transfer(x, got_bits)
  exp_bits = transfer(tiny(1.0_real32) * epsilon(1.0_real32), exp_bits)
  if (got_bits /= exp_bits) error stop 'long internal real token parsed incorrectly'

  print *, 'PASS'
end program read_61
