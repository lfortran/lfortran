module implicit_interface_108_mod
    implicit none
    external :: implicit_interface_108_add
contains
    subroutine twice(x)
        integer, intent(inout) :: x
        x = 2*x
    end subroutine
end module

subroutine implicit_interface_108_inc(x)
    integer, intent(inout) :: x
    x = x + 1
end subroutine

integer function implicit_interface_108_two()
    implicit_interface_108_two = 2
end function

subroutine implicit_interface_108_add(x, y)
    integer, intent(inout) :: x
    integer, intent(in) :: y
    x = x + y
end subroutine

program implicit_interface_108
    use implicit_interface_108_mod, only: implicit_interface_108_add, twice
    implicit none
    external :: implicit_interface_108_inc
    integer, external :: implicit_interface_108_two
    interface
        subroutine s(x)
            integer, intent(inout) :: x
        end subroutine
    end interface
    procedure(s), pointer :: p
    integer :: x

    x = 1
    call implicit_interface_108_inc(x)
    if (x /= 2) error stop

    if (implicit_interface_108_two() /= 2) error stop
    x = x + implicit_interface_108_two()
    if (x /= 4) error stop

    call implicit_interface_108_add(x, 3)
    if (x /= 7) error stop

    p => twice
    call p(x)
    if (x /= 14) error stop

    print *, x
end program
