module type_bound_generic_pass_01_m
    implicit none
    type :: t
        integer :: n = 0
        real :: r = 0.0
    contains
        procedure, pass(x) :: set_int
        procedure :: set_real
        procedure, pass(x) :: set_two
        generic :: g => set_int, set_real, set_two
        procedure, pass(x) :: get_sum
        generic :: f => get_sum
    end type

    type, extends(t) :: t2
    contains
        procedure, pass(x) :: set_int => set_int_t2
    end type
contains
    subroutine set_int(n, x)
        integer, intent(in) :: n
        class(t), intent(inout) :: x
        x%n = n
    end subroutine

    subroutine set_real(x, r)
        class(t), intent(inout) :: x
        real, intent(in) :: r
        x%r = r
    end subroutine

    subroutine set_two(n, m, x)
        integer, intent(in) :: n, m
        class(t), intent(inout) :: x
        x%n = n + m
    end subroutine

    integer function get_sum(m, x)
        integer, intent(in) :: m
        class(t), intent(in) :: x
        get_sum = m + x%n
    end function

    subroutine set_int_t2(n, x)
        integer, intent(in) :: n
        class(t2), intent(inout) :: x
        x%n = 10 * n
    end subroutine
end module

program type_bound_generic_pass_01
    use type_bound_generic_pass_01_m
    implicit none
    type(t) :: a
    type(t2) :: b

    call a%g(2)
    if (a%n /= 2) error stop
    call a%g(3.5)
    if (abs(a%r - 3.5) > 1e-6) error stop
    call a%g(4, 5)
    if (a%n /= 9) error stop

    call b%g(2)
    if (b%n /= 20) error stop
    call b%g(1.5)
    if (abs(b%r - 1.5) > 1e-6) error stop

    call b%g(6, 1)
    if (b%n /= 7) error stop

    if (a%f(2) /= 11) error stop
    if (b%f(3) /= 10) error stop
    print *, "ok"
end program
