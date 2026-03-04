! Test procedure pointer in derived type with optional argument
module procedure_40_mod
    implicit none

    abstract interface
        subroutine callback(x, y)
            integer, intent(in) :: x
            integer, optional, intent(in) :: y
        end subroutine callback
    end interface

    type :: t
        procedure(callback), nopass, pointer :: cb => null()
    end type t

contains

    subroutine call_it(obj)
        type(t), intent(in) :: obj
        if (associated(obj%cb)) call obj%cb(1, 2)
    end subroutine call_it

end module procedure_40_mod

program procedure_40
    use procedure_40_mod
    implicit none
    type(t) :: obj

    ! Call with null pointer (should not crash)
    call call_it(obj)

    ! Associate and call
    obj%cb => my_callback
    call call_it(obj)

    print *, "ok"
contains
    subroutine my_callback(x, y)
        integer, intent(in) :: x
        integer, optional, intent(in) :: y
        if (x /= 1) error stop
        if (.not. present(y)) error stop
        if (y /= 2) error stop
    end subroutine my_callback
end program procedure_40
