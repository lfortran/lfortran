program arrays_elemental_16
    implicit none

    integer, parameter :: xi(0) = 1
    real, parameter :: xr(0) = 1.0

    call check_real(real(xi))
    call check_real(sin(xr))
    call check_int(int(xi))
    call check_int(abs(xi))

contains

    subroutine check_real(x)
        real, intent(in) :: x(:)
        if (size(x) /= 0) then
            print *, "Error: Expected size 0, got", size(x)
            error stop 1
        end if
    end subroutine check_real

    subroutine check_int(x)
        integer, intent(in) :: x(:)
        if (size(x) /= 0) then
            print *, "Error: Expected size 0, got", size(x)
            error stop 2
        end if
    end subroutine check_int

end program arrays_elemental_16
