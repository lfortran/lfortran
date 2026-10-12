module proc_ptr_nopass_10_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_nopass_10_m
    implicit none
    abstract interface
        subroutine cb(a)
            use proc_ptr_nopass_10_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fa(a)
        use proc_ptr_nopass_10_sizes, only: n
        real :: a(n)
        a = 5.0
    end subroutine
end module

module proc_ptr_nopass_10_u
    use proc_ptr_nopass_10_m, only: cb
    implicit none
    type :: t
        integer :: k = 7
        procedure(cb), pointer, nopass :: f => null()
    end type
end module

program proc_ptr_nopass_10
    use proc_ptr_nopass_10_u
    use proc_ptr_nopass_10_m, only: fa
    implicit none
    type(t) :: x
    real :: z(2)
    x%f => fa
    call x%f(z)
    print *, z, x%k
    if (any(z /= 5.0)) error stop
    if (x%k /= 7) error stop
end program
