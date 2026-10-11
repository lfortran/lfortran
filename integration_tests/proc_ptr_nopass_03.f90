module proc_ptr_nopass_03_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_nopass_03_m
    implicit none
    abstract interface
        subroutine cb1(a)
            use proc_ptr_nopass_03_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
    type t
        procedure(cb1), nopass, pointer :: g => null()
        procedure(cb2), nopass, pointer :: f => null()
    end type
    abstract interface
        subroutine cb2(x, a)
            use proc_ptr_nopass_03_sizes, only: n
            import t
            class(t) :: x
            real :: a(n)
        end subroutine
    end interface
    type(t) :: gx
contains
    subroutine fill(x, a)
        use proc_ptr_nopass_03_sizes, only: n
        class(t) :: x
        real :: a(n)
        a = 3.0
    end subroutine

    subroutine fill1(a)
        use proc_ptr_nopass_03_sizes, only: n
        real :: a(n)
        a = 4.0
    end subroutine
end module

program proc_ptr_nopass_03
    use proc_ptr_nopass_03_m, only: gx, fill, fill1
    implicit none
    real :: a(2)

    a = 0.0
    gx%f => fill
    call gx%f(gx, a)
    if (any(a /= 3.0)) error stop
    gx%g => fill1
    call gx%g(a)
    if (any(a /= 4.0)) error stop
    print *, a
end program
