module while_09_mod
    implicit none
    integer :: cnt = 0, nfin = 0, scnt = 0
    type :: pt
        integer :: a = 1
    contains
        final :: fin
    end type
contains
    subroutine fin(p)
        type(pt), intent(inout) :: p
        nfin = nfin + 1
    end subroutine

    function mk() result(p)
        type(pt) :: p
        cnt = cnt + 1
        p%a = cnt
    end function

    logical(1) function chk(p)
        type(pt), intent(in) :: p
        chk = p%a < 4
    end function

    function ff() result(s)
        character(len=:), allocatable :: s
        scnt = scnt + 1
        if (scnt < 4) then
            s = "go"
        else
            s = "stop"
        end if
    end function
end module

program while_09
    ! logical(1) DO WHILE conditions whose evaluation needs temporaries
    use while_09_mod
    implicit none
    integer :: n

    n = 0
    do while (chk(mk()))
        n = n + 1
    end do
    print *, n
    if (n /= 3) error stop
    if (cnt /= 4) error stop

    n = 0
    do while (logical(ff() == "go", 1))
        n = n + 1
    end do
    print *, n
    if (n /= 3) error stop
    if (scnt /= 4) error stop
end program
