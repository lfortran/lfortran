program arrays_13
    implicit none

    integer :: u, v
    integer, pointer :: i(:)
    real, pointer :: r(:, :)

    integer, target :: iv(2)
    real, target :: rv(2, 3)

    i => iv
    r => rv

    i(1) = 1
    if( i(1) /= 1 ) then
        i(2) = 3
    else
        i(2) = 7
    end if

    if( iv(1) /= 1 ) error stop
    if( iv(2) /= 7 ) error stop

    do u = lbound(r, 1), ubound(r, 1)
        do v = lbound(r, 2), ubound(r, 2)
            rv(u, v) = u * v
        end do
    end do

    call check_real(r)

contains

    subroutine check_real(r)
        real, pointer :: r(:, :)
        integer :: u, v
        do u = lbound(r, 1), ubound(r, 1)
            do v = lbound(r, 2), ubound(r, 2)
                if( r(u, v) /= u * v ) error stop
            end do
        end do
    end subroutine

end program

