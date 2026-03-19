program intrinsics_438
! Test norm2 with variable (non-constant) dim argument
implicit none

integer, parameter :: sp = kind(1.0)
real(sp) :: b(2, 2), res1(2), res2(2)
real(sp), parameter :: tol = 1.0e-6_sp
integer :: dim

b = reshape([1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp], shape(b))

! Test with variable dim in a loop
do dim = 1, 2
    if (dim == 1) then
        res1 = norm2(b, dim)
        print *, res1
        ! norm2 along dim=1: [sqrt(1+4), sqrt(9+16)] = [sqrt(5), 5]
        if (abs(res1(1) - sqrt(5.0_sp)) > tol) error stop
        if (abs(res1(2) - 5.0_sp) > tol) error stop
    else
        res2 = norm2(b, dim)
        print *, res2
        ! norm2 along dim=2: [sqrt(1+9), sqrt(4+16)] = [sqrt(10), sqrt(20)]
        if (abs(res2(1) - sqrt(10.0_sp)) > tol) error stop
        if (abs(res2(2) - sqrt(20.0_sp)) > tol) error stop
    end if
end do

print *, "All norm2 variable dim tests passed."
end program
