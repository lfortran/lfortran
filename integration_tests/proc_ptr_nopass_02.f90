module proc_ptr_nopass_02_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_nopass_02_m
    implicit none
    type t
        procedure(callback), nopass, pointer :: f => null()
    end type

    abstract interface
        subroutine callback(x, a)
            use proc_ptr_nopass_02_sizes, only: n
            import t
            class(t) :: x
            real :: a(n)
        end subroutine
    end interface

contains

    subroutine fill(x, a)
        use proc_ptr_nopass_02_sizes, only: n
        class(t) :: x
        real :: a(n)
        a = 3.0
    end subroutine
end module

program proc_ptr_nopass_02
    use proc_ptr_nopass_02_m
    implicit none
    type(t) :: x
    real :: a(2)

    a = 0.0
    x%f => fill
    if (.not. associated(x%f)) error stop
    call x%f(x, a)
    if (any(a /= 3.0)) error stop
    print *, a
end program
