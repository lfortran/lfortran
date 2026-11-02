! Arrays of strings whose dimensions are given by a `dimension` statement
! before their type declaration. Each procedure names its array differently
! (#15713), and the module has no `implicit none` (#16169).
module attr_dim_06_mod
contains

subroutine assumed_shape(b1)
    dimension b1(:)
    character(len=*) :: b1
    if (size(b1) /= 3) error stop
    if (len(b1) /= 2) error stop
    if (b1(2) /= 'de') error stop
end subroutine

subroutine explicit_shape(b2, n)
    integer :: n
    dimension b2(n)
    character(len=2) :: b2
    if (size(b2) /= 3) error stop
    if (b2(3) /= 'gh') error stop
end subroutine

subroutine assumed_size(b3)
    dimension b3(*)
    character(len=2) :: b3
    if (b3(1) /= 'ab') error stop
end subroutine

subroutine automatic(n)
    integer :: n
    dimension c(n)
    character(len=3) :: c
    c = 'xyz'
    c(2) = 'uvw'
    if (size(c) /= n) error stop
    if (c(1) /= 'xyz' .or. c(2) /= 'uvw') error stop
end subroutine

end module

program attr_dim_06
use attr_dim_06_mod
implicit none
character(len=2) :: x(3)
x = ['ab', 'de', 'gh']
call assumed_shape(x)
call explicit_shape(x, 3)
call assumed_size(x)
call automatic(2)
print *, x
end program
