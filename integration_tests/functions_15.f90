module module_1
implicit none

contains

    subroutine sub_1(a, b, c, d, e, f)
        integer, intent(inout) :: a
        real, intent(inout) :: b
        logical, intent(in) :: c
        real, intent(in), optional :: d
        integer, intent(in), optional :: e
        real, intent(in) :: f
        if (present(d)) then
            a = b ** 2 + d
            if (present(e)) then
                b = b * f + e
            else
                b = b * f
            end if
        else
            a = b ** 2
            if (present(e)) then
                b = b * f + e
            else
                b = b * f
            end if
        end if
    end subroutine sub_1
end module module_1

program functions_15
use module_1, only: sub_1
implicit none

    integer :: a
    real :: b
    a = 1
    b = 2
    call sub_1(a, b, .true., d=12.0, e=5, f=6.0)
    call sub_1(a, b, .false., f=6.0)
    call sub_1(a, b, .true., e=78, f=117.0)
    call sub_1(a, b, .false., d=58.0, f=123.0)
    print *, a, b
    if (a /= 144288208) error stop
    if (b /= 1477476.00) error stop

    print *, f(), f(42)      ! "f t"

contains

    logical function f(x)
        integer, intent(in), optional :: x
        f = present(x)
    end function

end program functions_15
