! gpu decline reason: LocalityElementType
! An array of LOCAL locality is a new array in each iteration. Each thread
! needs a copy of its own, and an array of derived type is not one a thread
! can hold as a local yet: one buffer shared by every thread would be a
! race, so the loop is not offloaded.
program gpu_decline_17
implicit none
type :: pair
    real :: a
end type
integer, parameter :: n = 1000
real :: x(n), g(n)
type(pair) :: t(2)
integer :: j
do j = 1, n
    x(j) = real(j)
end do
do concurrent (j = 1:n) local(t)
    t(1)%a = x(j)
    t(2)%a = 2.0 * x(j)
    g(j) = t(1)%a + t(2)%a
end do
do j = 1, n
    if (abs(g(j) - 3.0 * x(j)) > 1.0e-3) error stop
end do
print *, "ok"
end program
