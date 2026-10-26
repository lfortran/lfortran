! A generic merged from two modules calls its own specific, not the
! procedure of the host that has its name.
module modules_84_m1
    implicit none
    private
    public :: g

    interface g
        module procedure s1
    end interface

contains

    subroutine s1(x)
        integer, intent(out) :: x
        x = 1
    end subroutine

end module

module modules_84_m2
    implicit none
    private
    public :: g

    interface g
        module procedure s2
    end interface

contains

    subroutine s2(x)
        real, intent(out) :: x
        x = 2
    end subroutine

end module

module modules_84_host
    implicit none

contains

    subroutine s2(x)
        real, intent(out) :: x
        x = 100
    end subroutine

    subroutine run()
        use modules_84_m1
        use modules_84_m2
        integer :: i
        real :: r
        call g(i)
        call g(r)
        if (i /= 1) error stop
        if (abs(r - 2.0) > 1e-6) error stop
        print *, i, r
    end subroutine

end module

program modules_84
    use modules_84_host
    implicit none
    call run()
end program
