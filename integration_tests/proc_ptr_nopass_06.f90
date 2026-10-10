module proc_ptr_nopass_06_m
    implicit none
    integer :: n = 2
    type t
        procedure(callback), nopass, pointer :: f => null()
    end type
    abstract interface
        subroutine callback(x, a)
            import t, n
            class(t) :: x
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fill(x, a)
        class(t) :: x
        real :: a(n)
        a = 3.0
    end subroutine
end module

program proc_ptr_nopass_06
    use proc_ptr_nopass_06_m, only: t, fill
    implicit none
    type(t) :: x
    real :: a(2)
    integer :: n
    n = 5
    a = 0.0
    x%f => fill
    call x%f(x, a)
    if (any(a /= 3.0)) error stop
    if (n /= 5) error stop
    print *, a
end program
