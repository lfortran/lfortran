! A pure procedure whose dummy procedure is declared with a pure interface
! block, called from a program that also calls an external through an implicit
! interface. The dummy procedure is not a dummy data object, so it needs no
! INTENT (C1592 does not apply to it).
module implicit_interface_97_m
    implicit none
contains
    pure real function square(y)
        real, intent(in) :: y
        square = y*y
    end function

    pure real function use_interface_dummy(f, x)
        interface
            pure real function f(y)
                real, intent(in) :: y
            end function
        end interface
        real, intent(in) :: x
        use_interface_dummy = f(x) + 1
    end function

    pure subroutine apply_dummy(s, x, y)
        interface
            pure subroutine s(a, b)
                real, intent(in) :: a
                real, intent(out) :: b
            end subroutine
        end interface
        real, intent(in) :: x
        real, intent(out) :: y
        call s(x, y)
    end subroutine

    pure subroutine halve(a, b)
        real, intent(in) :: a
        real, intent(out) :: b
        b = a/2
    end subroutine
end module

program implicit_interface_97
    use implicit_interface_97_m
    implicit none
    external :: add_one
    real :: x, y
    x = 2
    call add_one(x)
    if (abs(use_interface_dummy(square, x) - 10.0) > 1e-5) error stop 1
    call apply_dummy(halve, x, y)
    if (abs(y - 1.5) > 1e-5) error stop 2
    print *, use_interface_dummy(square, x), y
end program

subroutine add_one(y)
    real :: y
    y = y + 1
end subroutine
