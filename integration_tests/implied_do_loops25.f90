module implied_do_loops25_mod
  implicit none
  type :: t
    real, allocatable :: vals(:)
  contains
    procedure :: values => get_values
  end type
contains
  pure function get_values(self) result(v)
    class(t), intent(in) :: self
    real, allocatable :: v(:)
    v = self%vals
  end function
end module

program implied_do_loops25
  use implied_do_loops25_mod
  implicit none
  integer, parameter :: n = 5
  type(t), allocatable :: a(:)
  real, allocatable :: res(:)
  integer :: i

  a = [(t([real(i)]), i=1,n)]
  res = [(a(i)%values(), i=1,n)]

  if (size(res) /= 5) error stop
  do i = 1, n
    if (abs(res(i) - real(i)) > 1.0e-6) error stop
  end do

  print *, "PASS"
end program
