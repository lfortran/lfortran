! An operator merged from two modules calls its own specifics, not the
! procedures of the program that have their names.
module modules_83_a
    implicit none
    private
    public :: operator(.plus.)

    interface operator(.plus.)
        module procedure plus_i
    end interface

contains

    integer function plus_i(a, b)
        integer, intent(in) :: a, b
        plus_i = a + 2*b
    end function

end module

module modules_83_b
    implicit none
    private
    public :: operator(.plus.)

    interface operator(.plus.)
        module procedure plus_r
    end interface

contains

    real function plus_r(a, b)
        real, intent(in) :: a, b
        plus_r = a + 3*b
    end function

end module

program modules_83
    use modules_83_a
    use modules_83_b
    implicit none
    integer :: k
    real :: r

    k = 1 .plus. 2
    r = 1.0 .plus. 2.0
    if (k /= 5) error stop
    if (abs(r - 7.0) > 1e-6) error stop
    if (plus_i(1) /= 100) error stop
    if (plus_r(1) /= 1000) error stop
    print *, k, r, plus_i(1), plus_r(1)

contains

    integer function plus_i(i)
        integer, intent(in) :: i
        plus_i = 100*i
    end function

    integer function plus_r(i)
        integer, intent(in) :: i
        plus_r = 1000*i
    end function

end program
