program gpu_metal_375
! Without --realloc-lhs-arrays a component the loop writes in both branches
! of an `if` is not allocated automatically. With bounds checking on, the
! launch reports that the written component is not allocated, as the
! assignment on the CPU does, instead of crashing.
implicit none
type tt
    real, allocatable :: v(:)
end type
type(tt) :: t(4)
integer :: i
do concurrent (i = 1:4)
    if (mod(i, 2) == 0) then
        t(i) = k(1.0)
    else
        t(i) = k(2.0)
    end if
end do
print *, size(t(1)%v), size(t(2)%v)
contains
pure function k(s) result(r)
    real, intent(in) :: s
    type(tt) :: r
    allocate(r%v(3))
    r%v = s
end function
end program
