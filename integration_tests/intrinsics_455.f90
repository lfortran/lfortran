! Test that an INTRINSIC declaration in one program unit does not leak into
! a separate program unit, allowing a dummy argument with the same name to
! shadow the intrinsic.
program intrinsics_455
    implicit none
    intrinsic nint
    integer :: x
    x = nint(3.7)
    if (x /= 4) error stop
    print *, "PASS"
end program

integer function add_one(nint, i)
    implicit none
    integer, intent(in) :: nint, i
    add_one = nint + i + 1
end function
