! Array-section assignment inside a procedure that is inlined into a
! Metal kernel. The backend hoists each section into a pointer
! temporary; that temporary must inherit the address space of the array
! it points into (thread here, device for a kernel buffer) instead of
! being hardcoded to `device`.
program gpu_metal_233
implicit none
real :: d(3,3), c(4,3)
integer :: j, k
real :: expected

! Scalar assigned to a bounded and to an open-ended section.
d = 0.0
do concurrent (j = 1:3)
    d(:,j) = e(j, 3)
end do

do j = 1, 3
    do k = 1, 3
        if (j == k) then
            expected = 1.0
        else
            expected = 0.0
        end if
        if (abs(d(k,j) - expected) > 1.0e-6) error stop "unit vector"
    end do
end do

! Section-to-section copy with an array RHS: this used to be lowered as
! a pointer assignment, which type-checked and silently did nothing.
c = 0.0
do concurrent (j = 1:3)
    c(:,j) = shifted(j)
end do

do j = 1, 3
    if (abs(c(1,j) - 30.0) > 1.0e-6) error stop "shifted 1"
    if (abs(c(2,j) - 40.0) > 1.0e-6) error stop "shifted 2"
    if (abs(c(3,j) - real(j)) > 1.0e-6) error stop "shifted 3"
    if (abs(c(4,j) - real(j)) > 1.0e-6) error stop "shifted 4"
end do

print *, d(1,1), d(2,1), c(1,1), c(3,2)
print *, "ok"

contains

    pure function e(dir, length) result(unit_vector)
        integer, intent(in) :: dir, length
        real :: unit_vector(length)
        unit_vector(1:dir-1) = 0.0
        unit_vector(dir) = 1.0
        unit_vector(dir+1:) = 0.0
    end function

    pure function shifted(n) result(v)
        integer, intent(in) :: n
        real :: v(4), t(4)
        t(1) = 10.0
        t(2) = 20.0
        t(3) = 30.0
        t(4) = 40.0
        v = 0.0
        v(1:2) = t(3:4)
        v(3:4) = real(n)
    end function

end program
