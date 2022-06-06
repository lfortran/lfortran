module bindc_02b
use iso_c_binding, only: c_ptr, c_int
implicit none

interface
    ! void driver();
    subroutine driver() bind(c)
    end subroutine

    ! void print_ptr(int n, float *A);
    subroutine print_ptr(n, A) bind(c)
    import :: c_ptr, c_int
    integer(c_int), intent(in) :: n
    type(c_ptr), value, intent(in) :: A
    end subroutine
end interface

contains

    ! void callback(int n, float *A);
    subroutine callback(n, A) bind(c)
    integer(c_int), intent(in) :: n
    type(c_ptr), value, intent(in) :: A
    print *, "callback: calling print_ptr(n, A), n =", n
    call print_ptr(n, A)
    end subroutine

end module
