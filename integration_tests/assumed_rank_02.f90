module assumed_rank_02_mod
    implicit none
    type :: t
        integer :: v = 0
    end type
    interface t
        module procedure new_t
    end interface
contains
    type(t) function new_t(flag, a1)
        integer, intent(in) :: flag
        integer, intent(in), dimension(..) :: a1
        new_t%v = flag
    end function
end module

program assumed_rank_02
    use assumed_rank_02_mod
    implicit none
    integer :: arr(3)
    type(t) :: x, y
    arr = [10, 20, 30]
    x = t(1, 2)
    if (x%v /= 1) error stop
    y = t(42, arr)
    if (y%v /= 42) error stop
    print *, x%v, y%v
end program
