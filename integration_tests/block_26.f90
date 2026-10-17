program block_26
    ! An interface block in a BLOCK construct declares the external
    ! function total, which has an assumed-shape argument, while the host
    ! (the program here, the external subroutine host below) contains an
    ! internal function of the same name. Each call must reach its own
    ! function.
    implicit none
    interface
        subroutine host(x)
            integer, intent(in) :: x(3)
        end subroutine host
    end interface
    integer :: x(3)
    x = [1, 2, 3]
    if (total(x) /= 1000) error stop 1
    block
        interface
            integer function total(a)
                integer, intent(in) :: a(:)
            end function total
        end interface
        if (total(x) /= 6) error stop 2
    end block
    call host(x)
    print *, "ok"
contains
    integer function total(a)
        integer, intent(in) :: a(:)
        total = 1000
    end function total
end program block_26

subroutine host(x)
    implicit none
    integer, intent(in) :: x(3)
    if (total(x) /= 2000) error stop 3
    block
        interface
            integer function total(a)
                integer, intent(in) :: a(:)
            end function total
        end interface
        if (total(x) /= 6) error stop 4
    end block
contains
    integer function total(a)
        integer, intent(in) :: a(:)
        total = 2000
    end function total
end subroutine host

integer function total(a)
    implicit none
    integer, intent(in) :: a(:)
    total = sum(a)
end function total
