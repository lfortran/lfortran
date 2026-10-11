module proc_ptr_nopass_04_sizes
    integer :: n = 2
end module

module proc_ptr_nopass_04_m
    abstract interface
        subroutine callback(a)
            use proc_ptr_nopass_04_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
    type t
        procedure(callback), nopass, pointer :: f => null()
    end type
contains
    subroutine fill(a)
        use proc_ptr_nopass_04_sizes, only: n
        real :: a(n)
        a = 3.0
    end subroutine
end module

program proc_ptr_nopass_04
    use proc_ptr_nopass_04_m
    type(t) :: x
    real :: a(2)
    a = 0.0
    x%f => fill
    call x%f(a)
    if (any(a /= 3.0)) error stop
    print *, a
end program
