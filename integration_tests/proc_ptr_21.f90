module proc_ptr_21_sizes
    integer :: n = 2
end module

module proc_ptr_21_m
    abstract interface
        subroutine callback(a)
            use proc_ptr_21_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fill(a)
        use proc_ptr_21_sizes, only: n
        real :: a(n)
        a = 3.0
    end subroutine
end module

program proc_ptr_21
    use proc_ptr_21_m
    implicit none
    procedure(callback), pointer :: p
    real :: a(2)
    a = 0.0
    p => fill
    call p(a)
    if (any(a /= 3.0)) error stop
    print *, a
end program
