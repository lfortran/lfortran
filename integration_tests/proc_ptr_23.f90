module proc_ptr_23_sizes
    integer :: n = 2
end module

module proc_ptr_23_a
    implicit none
    abstract interface
        subroutine callback(a)
            use proc_ptr_23_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fill(a)
        use proc_ptr_23_sizes, only: n
        real :: a(n)
        a = 3.0
    end subroutine
end module

module proc_ptr_23_b
    use proc_ptr_23_a, only: callback
    implicit none
contains
    subroutine apply(cb, a)
        procedure(callback) :: cb
        real :: a(2)
        call cb(a)
    end subroutine
end module

program proc_ptr_23
    use proc_ptr_23_a, only: fill
    use proc_ptr_23_b, only: apply
    implicit none
    real :: a(2)
    a = 0.0
    call apply(fill, a)
    if (any(a /= 3.0)) error stop
    print *, a
end program
