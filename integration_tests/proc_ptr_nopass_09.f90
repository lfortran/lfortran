module proc_ptr_nopass_09_sizes
    implicit none
contains
    pure integer function nn(i)
        integer, intent(in) :: i
        nn = 2*i
    end function
end module

program proc_ptr_nopass_09
    use proc_ptr_nopass_09_sizes
    implicit none
    abstract interface
        subroutine cb(a)
            import :: nn
            real :: a(nn(1))
        end subroutine
    end interface
    type :: t
        procedure(cb), pointer, nopass :: f => null()
    end type
    type(t) :: x
    real :: z(2)
    x%f => fa
    z = 0
    call x%f(z)
    print *, z
    if (any(z /= 5.0)) error stop
contains
    subroutine fa(a)
        real :: a(2)
        a = 5.0
    end subroutine
end program
