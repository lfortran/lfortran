module proc_ptr_33_sizes
    implicit none
contains
    pure integer function nn(i)
        integer, intent(in) :: i
        nn = 2*i
    end function
end module

module proc_ptr_33_m
    use proc_ptr_33_sizes, only: nn
    implicit none
    abstract interface
        subroutine cb(a)
            import :: nn
            real :: a(nn(1))
        end subroutine
    end interface
contains
    subroutine fa(a)
        real :: a(nn(1))
        a = 5.0
    end subroutine
    subroutine s(z, k)
        real, intent(inout) :: z(2)
        integer, intent(out) :: k
        integer :: nn
        procedure(cb), pointer :: p
        nn = 42
        p => fa
        if (.not. associated(p)) error stop
        call p(z)
        k = nn
    end subroutine
end module

program proc_ptr_33
    use proc_ptr_33_m
    implicit none
    real :: z(2)
    integer :: k
    z = 0
    call s(z, k)
    print *, z, k
    if (any(z /= 5.0)) error stop
    if (k /= 42) error stop
end program
