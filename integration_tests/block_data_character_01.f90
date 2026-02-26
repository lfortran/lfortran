program bdata_3
  implicit none

  call test ()

end program

block data bd
  implicit none

! Integers work
  integer :: i1
  common /blocki/ i1
  data i1 /12345/

! Characters now work with the fix
  character(5) :: c1
  common /blockc/ c1
  data c1 /'12345'/

end block data

subroutine test ()
  implicit none

  integer :: i1
  common /blocki/ i1

  character(5) :: c1
  common /blockc/ c1

  if (i1 /= 12345) error stop
  if (c1 /= '12345') error stop
  print *, 'PASSED: BLOCK DATA character initialization'

end subroutine
