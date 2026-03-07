program implied_do_loops21
! Test: allocatable array assigned from implied-do array constructor
! containing calls to a function returning an allocatable array,
! with --realloc-lhs-arrays enabled.
implicit none
integer :: k
real, allocatable :: x(:)

x = [(f(k), k=1,3)]
if (size(x) /= 3) error stop
if (abs(x(1) - 1.0) > 1e-6) error stop
if (abs(x(2) - 2.0) > 1e-6) error stop
if (abs(x(3) - 3.0) > 1e-6) error stop

contains

pure function f(j) result(v)
    integer, intent(in) :: j
    real, allocatable :: v(:)
    v = [real(j)]
end function

end program
