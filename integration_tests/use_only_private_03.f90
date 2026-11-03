module use_only_private_03_m
    implicit none
    private
    public :: g
    interface g
        module procedure s
    end interface
contains
    subroutine s(x)
        integer, intent(out) :: x
        x = 5
    end subroutine
end module

program use_only_private_03
    use use_only_private_03_m, only: g
    implicit none
    integer :: x
    call g(x)
    if (x /= 5) error stop
    print *, x
end program
