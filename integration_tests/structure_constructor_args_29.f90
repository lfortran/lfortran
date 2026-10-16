! A structure constructor can give a procedure pointer component a procedure
! or procedure pointer with the characteristics of the component's explicit
! interface, or null(); a pure procedure is allowed for an interface that is
! not pure (Fortran 2018 7.5.10, 10.2.2.4).
module structure_constructor_args_29_mod
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

end module structure_constructor_args_29_mod

program structure_constructor_args_29
    use structure_constructor_args_29_mod
    implicit none
    type(procs_t) :: p
    type(self_t) :: s
    procedure(int_sub_i), pointer :: q

    p = procs_t(set_seen, scaled_sum, pad3)
    call p%sub(7)
    if (seen /= 7) error stop 1
    if (abs(p%fn(2.0, [1.0, 2.0, 3.0]) - 12.0) > 1e-6) error stop 2
    if (p%cfn(42) /= "042") error stop 3

    ! A procedure pointer with the same interface, and null().
    q => set_seen
    p = procs_t(q, scaled_sum, pad3)
    call p%sub(9)
    if (seen /= 9) error stop 4
    p = procs_t(sub=null(), cfn=pad3, fn=scaled_sum)
    if (associated(p%sub) .or. .not. associated(p%cfn)) error stop 5

    s = self_t(21, twice_v)
    if (s%get() /= 42) error stop 6
    print *, "ok"
end program structure_constructor_args_29
