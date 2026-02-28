module assumed_rank_03_mod
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
        class(*), intent(in), dimension(..) :: a1
        new_t%v = flag
    end function
end module

program assumed_rank_03
    use assumed_rank_03_mod
    implicit none
    type(t) :: x
    x = t(1, 'hello')
    if (x%v /= 1) error stop
    x = t(42, 123)
    if (x%v /= 42) error stop
    print *, "ok"
end program
