module proc_ptr_22_sizes
    integer :: n = 2
end module

module proc_ptr_22_a
    implicit none
    abstract interface
        subroutine callback(a)
            use proc_ptr_22_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fill(a)
        use proc_ptr_22_sizes, only: n
        real :: a(n)
        a = 3.0
    end subroutine
end module

module proc_ptr_22_b
    use proc_ptr_22_a
    implicit none
    procedure(callback), pointer :: p => null()
end module

program proc_ptr_22
    use proc_ptr_22_b
    implicit none
    real :: a(2)
    a = 0.0
    p => fill
    call p(a)
    if (any(a /= 3.0)) error stop
    print *, a
end program
