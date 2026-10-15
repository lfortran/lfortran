! Procedures with the characteristics of a procedure pointer component's
! explicit interface (a pure one for an interface that is not pure, a passed
! class(t) object) are allowed in initializer structure constructors
! (Fortran 2018 7.5.10, 10.2.2.4).
module structure_constructor_args_30_mod
    implicit none
    integer :: seen = 0

    abstract interface
        subroutine int_sub_i(i)
            integer, intent(in) :: i
        end subroutine int_sub_i
        real function real_fn_i(x, a)
            real, intent(in) :: x
            real, intent(in) :: a(:)
        end function real_fn_i
        function char_fn_i(n) result(r)
            integer, intent(in) :: n
            character(len=3) :: r
        end function char_fn_i
    end interface

    type :: procs_t
        procedure(int_sub_i), pointer, nopass :: sub => null()
        procedure(real_fn_i), pointer, nopass :: fn => null()
        procedure(char_fn_i), pointer, nopass :: cfn => null()
    end type procs_t

    type :: self_t
        integer :: v = 0
        procedure(self_i), pointer :: get => null()
    end type self_t

    abstract interface
        integer function self_i(self)
            import :: self_t
            class(self_t), intent(in) :: self
        end function self_i
    end interface

contains

    subroutine set_seen(i)
        integer, intent(in) :: i
        seen = i
    end subroutine set_seen

    pure real function scaled_sum(x, a)
        real, intent(in) :: x
        real, intent(in) :: a(:)
        scaled_sum = x * sum(a)
    end function scaled_sum

    function pad3(n) result(r)
        integer, intent(in) :: n
        character(len=3) :: r
        write (r, "(i3.3)") n
    end function pad3

    integer function twice_v(self)
        class(self_t), intent(in) :: self
        twice_v = 2 * self%v
    end function twice_v

end module structure_constructor_args_30_mod

program structure_constructor_args_30
    use structure_constructor_args_30_mod
    implicit none
    type(procs_t) :: saved = procs_t(set_seen, scaled_sum, pad3)
    type(self_t) :: saved_self = self_t(5, twice_v)

    call saved%sub(11)
    if (seen /= 11) error stop 1
    if (abs(saved%fn(0.5, [4.0]) - 2.0) > 1e-6) error stop 2
    if (saved%cfn(5) /= "005") error stop 3
    if (saved_self%v /= 5) error stop 4
    if (saved_self%get() /= 10) error stop 5
    print *, "ok"
end program structure_constructor_args_30
