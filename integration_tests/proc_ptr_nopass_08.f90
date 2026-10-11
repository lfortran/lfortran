module proc_ptr_nopass_08_sizes
    implicit none
contains
    pure integer function nn(i)
        integer, intent(in) :: i
        nn = 2*i
    end function
end module

module proc_ptr_nopass_08_m
    implicit none
    abstract interface
        subroutine cb(a)
            use proc_ptr_nopass_08_sizes, only: nn
            real :: a(nn(1))
        end subroutine
    end interface
contains
    subroutine fa(a)
        use proc_ptr_nopass_08_sizes, only: nn
        real :: a(nn(1))
        a = 5.0
    end subroutine
end module

module proc_ptr_nopass_08_u
    use proc_ptr_nopass_08_m
    implicit none
    type :: t
        procedure(cb), pointer, nopass :: f => null()
        integer :: nn = 7
    end type
end module

program proc_ptr_nopass_08
    use proc_ptr_nopass_08_u
    implicit none
    type(t) :: x
    real :: z(2)
    x%f => fa
    call x%f(z)
    print *, z, x%nn
    if (any(z /= 5.0)) error stop
    if (x%nn /= 7) error stop
end program
