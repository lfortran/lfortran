module proc_ptr_29_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_29_m
    implicit none
    abstract interface
        subroutine cb(a)
            use proc_ptr_29_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fa(a)
        use proc_ptr_29_sizes, only: n
        real :: a(n)
        a = 2.0
    end subroutine
end module

program proc_ptr_29
    use proc_ptr_29_m
    implicit none
    procedure(cb), pointer :: p
    real :: z(2)
    p => fa
    z = 0.0
    call t()
    print *, z
    if (any(z /= 2.0)) error stop
contains
    subroutine t()
        call p(z)
    end subroutine
end program
