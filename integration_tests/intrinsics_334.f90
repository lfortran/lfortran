program intrinsics_334
    implicit none

    integer, allocatable :: from(:), to(:)

    allocate(from(5))
    from = [1, 2, 3, 4, 5]

    allocate(to(5))
    call move_alloc(from, to)
    print *, to
    if(any(to /= [1,2,3,4,5])) error stop

    ! this check doesn't return correct value for now, as we don't deallocte `from` variable yet.

    ! print *, allocated(from)
    ! if(allocated(from) .neqv. .false.) error stop 

end program
