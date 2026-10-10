module proc_ptr_32_sizes
    implicit none
contains
    pure integer function nn(i)
        integer, intent(in) :: i
        nn = 2*i
    end function
end module

module proc_ptr_32_m
    implicit none
    abstract interface
        subroutine cb(a)
            use proc_ptr_32_sizes, only: nn
            real :: a(nn(1))
        end subroutine
    end interface
contains
    subroutine fa(a)
        real :: a(2)
        a = 5.0
    end subroutine
end module

module proc_ptr_32_user
    use proc_ptr_32_m
    implicit none
    procedure(cb), pointer :: p
contains
    subroutine run(z)
        real, intent(inout) :: z(2)
        p => fa
        call p(z)
    end subroutine
    integer function nn()
        nn = 7
    end function
end module

program proc_ptr_32
    use proc_ptr_32_user
    implicit none
    real :: z(2)
    call run(z)
    print *, z, nn()
    if (any(z /= 5.0)) error stop
    if (nn() /= 7) error stop
end program
