! A procedure pointer function result whose interface has an explicit-shape
! dummy with a use-associated extent (#14492)
module proc_ptr_42_sizes
    implicit none
    integer :: n = 2
end module

module proc_ptr_42_m
    implicit none
    abstract interface
        subroutine cb(a)
            use proc_ptr_42_sizes, only: n
            real, intent(inout) :: a(n)
        end subroutine
    end interface
contains
    subroutine g(a)
        use proc_ptr_42_sizes, only: n
        real, intent(inout) :: a(n)
        a = 9
    end subroutine

    function getp() result(p)
        procedure(cb), pointer :: p
        p => g
    end function
end module

program proc_ptr_42
    use proc_ptr_42_m
    implicit none
    procedure(cb), pointer :: q
    real :: a(2)
    a = 0
    q => getp()
    call q(a)
    if (any(a /= 9)) error stop
    print *, a
end program
