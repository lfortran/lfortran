module lbound_05_m
    implicit none
    integer, target :: lbound_05_t(4:8, -1:1) = 0
contains
    function lbound_05_f() result(r)
        integer, pointer :: r(:,:)
        r => lbound_05_t
    end function
end module

program lbound_05
    ! A reference to a function with a pointer result is not a whole array,
    ! so lbound gives 1 and ubound gives the extent, not the target's bounds.
    use lbound_05_m
    implicit none
    if (lbound(lbound_05_f(), 1) /= 1) error stop 1
    if (ubound(lbound_05_f(), 2) /= 3) error stop 2
    if (any(lbound(lbound_05_f()) /= [1, 1])) error stop 3
    if (any(ubound(lbound_05_f()) /= [5, 3])) error stop 4
    print *, "PASS"
end program
