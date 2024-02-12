program arrays_40
    implicit none

    real(4), dimension(2) :: z
    z = foo_sp_arr()

    print *, z
    if( any(z /= [2.0_4, -2.0_4]) ) error stop

contains

    function foo_sp_arr(x) result(z)
        real(4), dimension(2), intent(in), optional :: x
        real(4), dimension(2) :: z
        z = optval_rsp(x, [2.0_4, -2.0_4])
    end function foo_sp_arr

    pure elemental function optval_rsp(x, default) result(y)
        real(4), intent(in), optional :: x
        real(4), intent(in) :: default
        real(4) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_rsp

end program arrays_40
