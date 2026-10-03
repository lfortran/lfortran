program gpu_metal_86
! Test: indexing allocatable array member of derived type passed to a
! function called from do concurrent (Metal GPU offload).
! The allocatable data must be passed as a separate device buffer so
! that the Metal inline function can subscript it.
implicit none

type :: t
    integer, allocatable :: a(:)
end type

type(t) :: x
integer :: r(1), i

allocate(x%a, source=[7])

do concurrent (i = 1:1)
    r(i) = f(x)
end do

if (r(1) /= 7) error stop

print *, "ok"

contains

pure integer function f(s)
    type(t), intent(in) :: s
    f = s%a(1)
end function

end program
