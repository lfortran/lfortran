module optional_03_m
contains

    subroutine my_sub(a)
        integer, intent(in) :: a
        print *, "a =", a
    end subroutine my_sub

    function f1(x) result(r)
        integer, optional, intent(in) :: x
        integer, allocatable :: r
        r = 0
    end function f1
end module

program optional_03
    use optional_03_m
    integer, allocatable :: i
    i = 10
    ! Fails for NUM_TRIES < 4
    call my_sub(f1(f1(f1(i))))
end program
