module use_05_mod
    implicit none
contains
    subroutine incr(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine incr

    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function twice
end module use_05_mod

program use_05
    ! Procedures imported under another name are called by that name.
    use use_05_mod, only: bump => incr, double => twice
    implicit none
    integer :: i
    i = 1
    call bump(i)
    if (i /= 2) error stop
    if (double(i) /= 4) error stop
    print *, i, double(i)
end program use_05
