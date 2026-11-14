module intent_out_polymorphic_dyn_07_m
    implicit none
    type :: inner
        character(len=5) :: c = "hello"
    end type
    type :: base
        integer :: h = 1
    end type
    type, extends(base) :: ext
        type(inner), allocatable :: is
        type(inner), allocatable :: ia(:)
        type(inner) :: keep
    end type
contains
    subroutine reset(v)
        class(base), intent(out) :: v
    end subroutine

    subroutine reset_array(v)
        class(base), intent(out) :: v(:)
    end subroutine
end module

program intent_out_polymorphic_dyn_07
    use intent_out_polymorphic_dyn_07_m
    implicit none
    type(ext) :: x
    type(ext) :: y(2)
    integer :: i

    x%h = 5
    x%keep%c = "world"
    allocate(x%is)
    allocate(x%ia(3))
    x%is%c = "abcde"
    x%ia(2)%c = "fghij"
    call reset(x)
    if (x%h /= 1) error stop 1
    if (allocated(x%is)) error stop 2
    if (allocated(x%ia)) error stop 3
    if (x%keep%c /= "hello") error stop 4

    allocate(x%is)
    allocate(x%ia(2))
    if (x%is%c /= "hello") error stop 5
    if (x%ia(1)%c /= "hello" .or. x%ia(2)%c /= "hello") error stop 6

    do i = 1, 2
        y(i)%h = 10 + i
        allocate(y(i)%is)
        allocate(y(i)%ia(i))
        y(i)%keep%c = "xyzzy"
    end do
    call reset_array(y)
    do i = 1, 2
        if (y(i)%h /= 1) error stop 7
        if (allocated(y(i)%is)) error stop 8
        if (allocated(y(i)%ia)) error stop 9
        if (y(i)%keep%c /= "hello") error stop 10
    end do

    deallocate(x%is, x%ia)
    print *, "ok"
end program
