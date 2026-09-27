! A program type (c_t) used as a component of another program type (d_t)
! and reached from a contained procedure through a host named constant and
! variable. nested_vars moves c_t into its context module, so the component
! 'c' of d_t, which stays in the program, must be re-pointed (#12939).
program nested_vars_15
  implicit none
  type :: c_t
    integer :: x = 1
  end type
  type :: d_t
    type(c_t) :: c
    integer :: y = 3
  end type
  type(c_t), parameter :: qc = c_t(70)
  type(c_t) :: h
  type(d_t) :: d
  h = c_t(5)
  call s()
  if (h%x /= 75) error stop 2
  d%c = h
  d%y = d%y + qc%x
  if (d%c%x /= 75 .or. d%y /= 73) error stop 3
  d = d_t(c_t(qc%x + 1), 4)
  if (d%c%x /= 71 .or. d%y /= 4) error stop 4
  print *, qc%x, h%x, d%c%x, d%y
contains
  subroutine s()
    if (qc%x /= 70) error stop 1
    h = c_t(h%x + qc%x)
  end subroutine
end program
