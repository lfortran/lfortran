module class_optional_03_mod
    implicit none
    type :: item_t
        integer :: v = 0
    end type
    type, extends(item_t) :: big_item_t
        integer :: w = 0
    end type
contains
    subroutine consumea(r, item)
        integer, intent(out) :: r
        class(item_t), optional :: item(:)
        r = -1
        if (present(item)) r = item(1)%v + item(2)%v
    end subroutine

    function makea(n) result(item)
        integer, intent(in) :: n
        class(item_t), allocatable :: item(:)
        allocate(big_item_t :: item(2))
        item(1)%v = n
        item(2)%v = 2*n
    end function
end module

program class_optional_03
    use class_optional_03_mod
    implicit none
    integer :: r
    call consumea(r, makea(3))
    if (r /= 9) error stop
    print *, "ok"
end program
