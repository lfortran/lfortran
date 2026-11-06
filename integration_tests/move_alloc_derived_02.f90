program move_alloc_derived_02
    implicit none
    type :: bag
        real, allocatable :: v(:)
    end type
    type(bag) :: shelf(2)
    type(bag), allocatable :: d(:)
    real, allocatable :: src(:)

    allocate(src(3))
    src = [1.0, 2.0, 3.0]
    call move_alloc(src, shelf(2)%v)
    if (allocated(src)) error stop
    if (.not. allocated(shelf(2)%v)) error stop
    if (any(shelf(2)%v /= [1.0, 2.0, 3.0])) error stop

    allocate(d(2))
    call move_alloc(shelf(2)%v, d(2)%v)
    if (allocated(shelf(2)%v)) error stop
    if (size(d(2)%v) /= 3) error stop
    if (d(2)%v(3) /= 3.0) error stop
    print *, "ok"
end program
