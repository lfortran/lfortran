! Declares derived-type parameters whose procedure-pointer components are
! null, both through the component default and through an explicit
! `null()`. The interfaces take a derived-type and a procedure argument.
module structure_constructor_args_08_a
    implicit none
    type :: d_t
        integer :: v = 3
    end type
    abstract interface
        integer function gi(i)
            integer, intent(in) :: i
        end function
        integer function fi(d, cb)
            import :: d_t, gi
            type(d_t), intent(in) :: d
            procedure(gi) :: cb
        end function
    end interface
    type :: o_t
        integer :: x = 5
        procedure(fi), pointer, nopass :: fp => null()
        procedure(gi), pointer, nopass :: gp => null()
    end type
    type(o_t), parameter :: z = o_t()
    type(o_t), parameter :: z2 = o_t(7, null(), null())
end module
