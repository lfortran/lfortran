module elemental_16_mymod
implicit none
private

    public :: sub_1d, sub_elemental

    contains

        pure subroutine sub_1d(xout, xin1, xin2)
            real, intent(out) :: xout(:)
            real, intent(in) :: xin1, xin2
            xout = 1.234*(xin2 - xin1) + xin1
        end subroutine sub_1d

        pure elemental subroutine sub_elemental(xout, xin1, xin2)
            real, intent(out) :: xout
            real, intent(in) :: xin1, xin2
            xout = 1.234*(xin2 - xin1) + xin1
        end subroutine sub_elemental

end module

program main
use, non_intrinsic :: elemental_16_mymod, only: sub_1d, sub_elemental
implicit none

    integer, parameter :: n = 3
    real :: v(n)

    v = -1.0
    write(*,*) 'v: ',v

    call sub_1d(v, 0.0, 1.0)
    write(*,*) 'v: ',v
    if (.not. all(abs(v - 1.234) < 1e-5)) error stop

    v = -1.0
    write(*,*) 'v: ',v

    call sub_elemental(v, 0.0, 1.0)
    write(*,*) 'v: ',v
    if (.not. all(abs(v - 1.234) < 1e-5)) error stop

end program main
