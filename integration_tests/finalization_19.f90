! The result of a function is not an INTENT(OUT) dummy argument: an array
! result of a type with a final subroutine is not finalized when the function
! is invoked. It is default initialized, like a scalar result.
module finalization_19_mod
    implicit none
    integer :: nfin = 0, nfin_before_call = -1
    type :: t
        integer :: v = -1
    contains
        final :: fin_t1
    end type
contains
    subroutine fin_t1(x)
        type(t), intent(inout) :: x(:)
        nfin = nfin + 1
    end subroutine

    function make(n) result(r)
        integer, intent(in) :: n
        type(t) :: r(n)
        if (nfin /= nfin_before_call) error stop 20
        if (any(r%v /= -1)) error stop 21
        r(1)%v = 7
    end function

    function make3() result(r)
        type(t) :: r(3)
        if (nfin /= nfin_before_call) error stop 22
        r(2)%v = 8
    end function

    subroutine run()
        type(t) :: y(3)
        nfin_before_call = nfin
        y = make(3)
        if (any(y%v /= [7, -1, -1])) error stop 1
        nfin_before_call = nfin
        y = make3()
        if (any(y%v /= [-1, 8, -1])) error stop 3
    end subroutine
end module

program finalization_19
    use finalization_19_mod
    implicit none
    call run()
    print *, "ok"
end program
