! Components of derived types used in a procedure with an assumed-shape
! dummy (which pass_array_by_data specialises) keep their mangled names (#16091)
module mangle_underscore_05_m
    implicit none
    type t
        logical :: flag = .true.
    end type
    type(t) :: c
contains
    subroutine show_mod(x)
        real, intent(in) :: x(:)
        if (.not. c%flag) error stop
        if (size(x) /= 1) error stop
    end subroutine
end module

program mangle_underscore_05
    use mangle_underscore_05_m, only: show_mod
    implicit none
    type u
        integer :: n = 42
    end type
    type w
        type(u) :: inner
        integer :: m = 7
    end type
    call show([1.0, 2.0])
    call show_mod([3.0])
    print *, "ok"
contains
    subroutine show(x)
        real, intent(in) :: x(:)
        type v
            integer :: k = 5
        end type
        type(u) :: a
        type(w) :: b
        type(v) :: d
        a%n = a%n + int(sum(x))
        if (a%n /= 45) error stop
        b = w(u(int(x(2))), 9)
        if (b%inner%n /= 2) error stop
        if (b%m /= 9) error stop
        if (d%k /= 5) error stop
        d = v(int(x(1)) + 10)
        if (d%k /= 11) error stop
    end subroutine
end program
