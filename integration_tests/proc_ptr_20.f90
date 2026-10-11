module proc_ptr_20_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_20_m
    implicit none
    abstract interface
        subroutine callback(a)
            use proc_ptr_20_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
    procedure(callback), pointer :: p => null()
contains
    subroutine fill(a)
        use proc_ptr_20_sizes, only: n
        real :: a(n)
        a = 3.0
    end subroutine
end module

program proc_ptr_20
    use proc_ptr_20_m
    implicit none
    real :: a(2)

    a = 0.0
    p => fill
    call p(a)
    if (any(a /= 3.0)) error stop
    print *, a
end program
