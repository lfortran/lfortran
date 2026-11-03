program legacy_array_sections_34
    implicit none
    integer :: dst(2), arr(4)
    dst = 0
    call add(dst(lv(1)))
    print *, dst
    if (dst(1) /= 0) error stop 1
    if (dst(2) /= 5) error stop 2

    arr = 0
    call add2(arr(lv(1)))
    print *, arr
    if (any(arr /= [0, 7, 8, 0])) error stop 3
contains
    integer function lv(i)
        integer, intent(in) :: i
        lv = i + 1
    end function
    subroutine add(x)
        integer, intent(inout) :: x(1)
        x(1) = 5
    end subroutine
    subroutine add2(x)
        integer, intent(inout) :: x(2)
        x(1) = 7
        x(2) = 8
    end subroutine
end program
