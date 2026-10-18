! An element of a named constant array associated with an array dummy of an
! external procedure (sequence association): the callee receives the element
! sequence that starts at that element in the storage of the named constant.
program parameter_28
implicit none
integer, parameter :: ip(4) = [10, 20, 30, 40]
real, parameter :: rp(2,3) = reshape([1., 2., 3., 4., 5., 6.], [2,3])
character(len=4), parameter :: chp(3) = ["abcd", "efgh", "ijkl"]
integer :: r
real :: s

call parameter_28_explicit(ip(2), r)
print *, r
if (r /= 90) error stop 1

call parameter_28_assumed(ip(3), 2, r)
print *, r
if (r /= 70) error stop 2

call parameter_28_real(rp(1,2), s)
print *, s
if (abs(s - 18.0) > 1e-6) error stop 3

call parameter_28_char_explicit(chp(2), r)
print *, r
if (r /= 3) error stop 4

call parameter_28_char_assumed(chp(1), r)
print *, r
if (r /= 7) error stop 5
end program
