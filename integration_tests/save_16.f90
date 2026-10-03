module save_16_mod
    implicit none
contains

    subroutine step_sub(val)
        integer, intent(out) :: val
        integer :: i = 3
        save :: i
        i = i + 1
        val = i
    end subroutine

    integer function step_func() result(res)
        integer :: count = 10
        save :: count
        count = count + 5
        res = count
    end function

    subroutine step_multi(x, y)
        integer, intent(out) :: x, y
        integer :: a = 1
        integer :: b = 100
        save :: a, b
        a = a + 2
        b = b + 20
        x = a
        y = b
    end subroutine

end module

program save_16
    use save_16_mod
    implicit none
    integer :: v, x, y

    call step_sub(v)
    if (v /= 4) error stop 1
    call step_sub(v)
    if (v /= 5) error stop 2

    if (step_func() /= 15) error stop 3
    if (step_func() /= 20) error stop 4

    call step_multi(x, y)
    if (x /= 3 .or. y /= 120) error stop 5
    call step_multi(x, y)
    if (x /= 5 .or. y /= 140) error stop 6

    print *, "OK"
end program
