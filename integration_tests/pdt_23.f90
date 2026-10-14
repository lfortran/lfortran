module len_parameter_inquiry
  implicit none
  type :: box(n)
    integer, len :: n
    integer :: value
  end type
contains
  subroutine check(n)
    integer, intent(in) :: n
    type(box(n)) :: a
    type(box(n+2)) :: b
    a%value = 13
    b%value = 29
    if (a%n /= n .or. b%n /= n+2) error stop
    call inspect(a, n, 13)
    call inspect(b, n+2, 29)
  end subroutine
  subroutine inspect(x, n, v)
    type(box(*)), intent(in) :: x
    integer, intent(in) :: n, v
    if (x%n /= n .or. x%value /= v) error stop
  end subroutine
end module
program test
  use len_parameter_inquiry
  call check(3)
  call check(7)
end program
