module proc_ptr_25_sa
    implicit none
    integer :: n = 2
end module

module proc_ptr_25_sb
    implicit none
    integer :: n = 3
end module

module proc_ptr_25_ia
    implicit none
    abstract interface
        subroutine cba(a)
            use proc_ptr_25_sa, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fa(a)
        use proc_ptr_25_sa, only: n
        real :: a(n)
        a = 2.0
    end subroutine
end module

module proc_ptr_25_ib
    implicit none
    abstract interface
        subroutine cbb(a)
            use proc_ptr_25_sb, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fb(a)
        use proc_ptr_25_sb, only: n
        real :: a(n)
        a = 3.0
    end subroutine
end module

program proc_ptr_25
    use proc_ptr_25_ia
    use proc_ptr_25_ib
    implicit none
    procedure(cba), pointer :: p
    procedure(cbb), pointer :: q, q2, q3
    real :: x(2), y(3)
    x = 0.0
    y = 0.0
    p => fa
    q => fb
    q2 => fb
    q3 => q2
    call p(x)
    call q(y)
    if (any(x /= 2.0)) error stop
    if (any(y /= 3.0)) error stop
    y = 0.0
    call q3(y)
    if (any(y /= 3.0)) error stop
    print *, x, y
end program
