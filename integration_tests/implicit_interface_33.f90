! Test: external subroutine call inside select type block
! with implicit interface compiles without verify error.
module implicit_interface_33_mod
    implicit none
contains
    subroutine process(value, res)
        class(*), intent(in) :: value
        integer, intent(out) :: res
        res = 0
        select type (value)
        type is (integer)
            res = value
        class default
            call set_default(res)
        end select
    end subroutine

    subroutine set_default(x)
        integer, intent(out) :: x
        x = 42
    end subroutine
end module

program implicit_interface_33
    use implicit_interface_33_mod
    implicit none
    integer :: r

    call process(10, r)
    if (r /= 10) error stop

    call process(3.14, r)
    if (r /= 42) error stop

    print *, "ok"
end program
