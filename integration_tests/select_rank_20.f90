module select_rank_20_mod
    implicit none

    type :: dt
        integer :: val
    end type

contains

    subroutine check(x)
        type(dt), intent(in) :: x(:)
        if (size(x) /= 3) error stop
        if (x(1)%val /= 10) error stop
        if (x(2)%val /= 20) error stop
        if (x(3)%val /= 30) error stop
        print *, "OK: size is", size(x)
    end subroutine

    subroutine wrap(x)
        type(dt), intent(in) :: x(..)
        select rank(x)
        rank(1)
            call check(x)
        rank default
            error stop "unexpected rank"
        end select
    end subroutine

end module

program select_rank_20
    use select_rank_20_mod
    implicit none
    type(dt) :: arr(3)
    arr = [dt(10), dt(20), dt(30)]
    call wrap(arr)
end program
