! `s(1:2) = h()`, where the internal function h writes another part of the
! allocatable character host variable s (#15309)
program nested_vars_31
    implicit none
    character(len=:), allocatable :: s
    s = "abcd"
    s(1:2) = h()
    print *, s
    if (s /= "xyzd") error stop
contains
    function h() result(r)
        character(len=2) :: r
        s(3:3) = "z"
        r = "xy"
    end function
end program
