! Whole-array assignment with a scalar RHS inside a procedure that is
! inlined into a Metal kernel. The MSL backend used to emit the Fortran
! whole-array assignment `v = 0.0` verbatim, which assigns a scalar to
! the bare pointer that represents the array. It must be expanded into
! an element loop that broadcasts the scalar instead.
program gpu_metal_218
implicit none
real :: d(4,3)
integer :: j, k
real :: expected

d = 1.0
do concurrent (j = 1:3)
    d(:,j) = filled(real(j))
end do

do j = 1, 3
    do k = 1, 4
        if (k == 2) then
            expected = real(j)
        else
            expected = 0.0
        end if
        if (abs(d(k,j) - expected) > 1.0e-6) error stop "filled"
    end do
end do

print *, d(2,1), d(2,3), d(1,1)
print *, "ok"

contains

    pure function filled(x) result(v)
        real, intent(in) :: x
        real :: v(4)
        v = 0.0
        v(2) = x
    end function

end program
