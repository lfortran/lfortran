program implied_do_loops20
! Test: reshape of array constructor with implied-do calling a function
! returning an allocatable array.
implicit none
integer :: k
real :: a(2, 2)

a = reshape([( f(k), k = 1, 4 )], [2, 2])

if (abs(a(1,1) - 1.0) > 1e-6) error stop
if (abs(a(2,1) - 2.0) > 1e-6) error stop
if (abs(a(1,2) - 3.0) > 1e-6) error stop
if (abs(a(2,2) - 4.0) > 1e-6) error stop

contains

pure function f(j) result(v)
    integer, intent(in) :: j
    real, allocatable :: v(:)
    allocate(v(1))
    v(1) = real(j)
end function

end program
