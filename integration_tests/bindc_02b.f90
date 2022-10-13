module bindc_02b
use iso_c_binding, only: c_ptr, c_int
implicit none

interface
    ! void driver();
    subroutine driver() bind(c)
    end subroutine

    ! void print_ptr1(int n, float *A);
    subroutine print_ptr1(n, A) bind(c)
    import :: c_ptr, c_int
    integer(c_int), value, intent(in) :: n
    type(c_ptr), value, intent(in) :: A
    end subroutine

    ! void print_ptr2(int *n, float *A);
    subroutine print_ptr2(n, A) bind(c)
    import :: c_ptr, c_int
    integer(c_int), intent(in) :: n
    type(c_ptr), value, intent(in) :: A
    end subroutine
end interface

contains

    ! void callback1(int n, float *A);
    subroutine callback1(n, A) bind(c)
    integer(c_int), value, intent(in) :: n
    type(c_ptr), value, intent(in) :: A
    print *, "callback1: calling print_ptr1(n, A), n =", n
    call print_ptr1(n, A)
    end subroutine

    ! void callback1b(int n, float *A);
    subroutine callback1b(n, A) bind(c)
    integer(c_int), value, intent(in) :: n
    type(c_ptr), value, intent(in) :: A
    print *, "callback1b: calling print_ptr2(n, A), n =", n
    call print_ptr2(n, A)
    end subroutine

    ! void callback2(int *n, float *A);
    subroutine callback2(n, A) bind(c)
    integer(c_int), intent(in) :: n
    type(c_ptr), value, intent(in) :: A
    print *, "callback2: calling print_ptr2(n, A), n =", n
    call print_ptr2(n, A)
    end subroutine

    ! void callback2b(int *n, float *A);
    subroutine callback2b(n, A) bind(c)
    integer(c_int), intent(in) :: n
    type(c_ptr), value, intent(in) :: A
    print *, "callback2b: calling print_ptr1(n, A), n =", n
    call print_ptr1(n, A)
    end subroutine

end module
