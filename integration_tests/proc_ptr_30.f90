module proc_ptr_30_sa
    implicit none
    integer :: n = 2
end module

module proc_ptr_30_sc
    implicit none
    integer :: n = 4
end module

module proc_ptr_30_ia
    implicit none
    abstract interface
        subroutine cba(a)
            use proc_ptr_30_sa, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fa(a)
        use proc_ptr_30_sa, only: n
        real :: a(n)
        a = 2.0
    end subroutine
end module

module proc_ptr_30_ic
    implicit none
    abstract interface
        subroutine cbc(a)
            use proc_ptr_30_sc, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fc(a)
        use proc_ptr_30_sc, only: n
        real :: a(n)
        a = 4.0
    end subroutine
end module

module proc_ptr_30_user
    use proc_ptr_30_ia
    implicit none
contains
    subroutine s(y)
        use proc_ptr_30_ic
        real, intent(inout) :: y(4)
        procedure(cbc), pointer :: r
        procedure(cba), pointer :: p2
        r => fc
        p2 => fa
        call r(y)
        call t()
    contains
        subroutine t()
            real :: z(2)
            z = 0.0
            call p2(z)
            y(1:2) = z
        end subroutine
    end subroutine
end module

program proc_ptr_30
    use proc_ptr_30_user
    implicit none
    real :: y(4)
    y = 0.0
    call s(y)
    print *, y
    if (any(y /= [2.0, 2.0, 4.0, 4.0])) error stop
end program
