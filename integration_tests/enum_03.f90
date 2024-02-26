program enum_03
    implicit none

    integer :: x
    enum, bind(c)
        enumerator :: bool = 999
    end enum

    call get_value(x)
    if (x /= 999) error stop

contains

    subroutine get_value(i)
        integer, intent(inout) :: i
        i = bool
    end subroutine get_value
end program enum_03
