module class_159_mod
    implicit none

    abstract interface
        subroutine real_out_iface(x)
            real, intent(out) :: x
        end subroutine
    end interface

    type :: pp_t
        procedure(real_out_iface), pointer, nopass :: random_number => null()
    end type

    type :: rng_t
        real :: v = 0.0
    contains
        procedure :: random_number => rng_random_number
    end type

contains

    subroutine fixed_value(x)
        real, intent(out) :: x
        x = 2.5
    end subroutine

    pure subroutine rng_random_number(rng, harvest)
        class(rng_t), intent(in) :: rng
        real, intent(out) :: harvest
        harvest = rng%v
    end subroutine

end module

program class_159
    use class_159_mod, only: pp_t, rng_t, fixed_value
    implicit none
    type(pp_t) :: o
    real :: y

    ! procedure-pointer component named like an intrinsic subroutine
    y = -1.0
    o%random_number => fixed_value
    call o%random_number(y)
    if (abs(y - 2.5) > 1e-6) error stop

    ! type-bound procedure named like an intrinsic, called from a pure procedure
    y = -1.0
    call pure_sub(5.0, y)
    if (abs(y - 5.0) > 1e-6) error stop
    print *, y

contains

    pure subroutine pure_sub(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        type(rng_t) :: rng
        rng%v = x
        call rng%random_number(y)
    end subroutine

end program
