module proc_ptr_27_sa
    implicit none
    integer :: n = 2
end module

module proc_ptr_27_sc
    implicit none
    integer :: n = 4
end module

module proc_ptr_27_ia
    implicit none
    abstract interface
        function cba(x) result(r)
            use proc_ptr_27_sa, only: n
            real, intent(in) :: x
            real :: r(n)
        end function
    end interface
contains
    function fa(x) result(r)
        use proc_ptr_27_sa, only: n
        real, intent(in) :: x
        real :: r(n)
        r = x
    end function
end module

module proc_ptr_27_ic
    implicit none
    abstract interface
        function cbc(x) result(r)
            use proc_ptr_27_sc, only: n
            real, intent(in) :: x
            real :: r(n)
        end function
    end interface
contains
    function fc(x) result(r)
        use proc_ptr_27_sc, only: n
        real, intent(in) :: x
        real :: r(n)
        r = x
    end function
end module

module proc_ptr_27_user
    use proc_ptr_27_ia
    use proc_ptr_27_ic, only: cbc, fc
    implicit none
contains
    subroutine s(y, k)
        real, intent(inout) :: y(:)
        integer, intent(out) :: k
        procedure(cba), pointer :: p2
        procedure(cbc), pointer :: r
        r => fc
        p2 => fa
        y = r(4.0)
        k = size(p2(2.0))
    end subroutine
end module

program proc_ptr_27
    use proc_ptr_27_user
    implicit none
    real :: y(4)
    integer :: k
    y = 0.0
    call s(y, k)
    print *, y, k
    if (any(y /= 4.0)) error stop
    if (k /= 2) error stop
end program
