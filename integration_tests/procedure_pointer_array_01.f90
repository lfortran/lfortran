module mre_array_ptr_mod
    implicit none

    abstract interface
        subroutine my_interface(args)
            real(8), intent(in) :: args(100)
        end subroutine my_interface
    end interface

    procedure(my_interface), pointer :: my_ptr => my_placeholder

contains

    subroutine my_placeholder(args)
        real(8), intent(in) :: args(100)
        if (abs(args(1) - 1.0_8) > 1e-10) then
            error stop "Value mismatch in procedure pointer call"
        end if
    end subroutine my_placeholder

    subroutine call_ptr()
        real(8) :: args(100)
        args(1) = 1.0_8
        call my_ptr(args)
    end subroutine call_ptr

end module mre_array_ptr_mod

program main
    use mre_array_ptr_mod
    implicit none
    call call_ptr()
end program main
