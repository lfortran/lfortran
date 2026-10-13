module proc_ptr_31_sizes
    implicit none
contains
    pure integer function nn(i)
        integer, intent(in) :: i
        nn = 2*i
    end function
end module

module proc_ptr_31_other
    implicit none
contains
    integer function nn(i)
        integer, intent(in) :: i
        nn = 100*i
    end function
end module

module proc_ptr_31_m
    implicit none
    abstract interface
        subroutine cb(a)
            use proc_ptr_31_sizes, only: nn
            real :: a(nn(1))
        end subroutine
    end interface
contains
    subroutine fa(a)
        use proc_ptr_31_sizes, only: nn
        real :: a(nn(1))
        a = 5.0
    end subroutine
end module

module proc_ptr_31_user
    use proc_ptr_31_m
    implicit none
    procedure(cb), pointer :: p
end module

program proc_ptr_31
    use proc_ptr_31_user
    use proc_ptr_31_other
    implicit none
    real :: z(2)
    integer :: k
    p => fa
    call p(z)
    k = nn(1)
    print *, z, k
    if (any(z /= 5.0)) error stop
    if (k /= 100) error stop
end program
