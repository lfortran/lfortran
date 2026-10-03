program allocate_72
    implicit none

    integer, allocatable :: a(:)
    integer :: stat_arr(2)

    type :: stat_holder
        integer :: stat
    end type stat_holder

    type(stat_holder) :: s

    stat_arr = -1
    s%stat = -1

    allocate(a(3), stat=stat_arr(2))
    if (stat_arr(2) /= 0) error stop "stat_arr"

    allocate(a(2), stat=stat_arr(1))
    print *, stat_arr(1)
    print *, size(a)
    if (stat_arr(1) == 0) error stop "stat_nonzero"
    if (.not. allocated(a)) error stop "stat_allocated"
    if (size(a) /= 3) error stop "stat_size"
    deallocate(a)

    allocate(a(2), stat=s%stat)
    if (s%stat /= 0) error stop "stat_member"
    deallocate(a)

    print *, "PASS"
end program allocate_72
