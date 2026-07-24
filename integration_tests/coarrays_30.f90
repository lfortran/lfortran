program coarrays_30
    implicit none

    ! 1. Two allocatable coarrays (one array, one scalar)
    integer, allocatable :: array_coarray(:)[:]
    integer, allocatable :: scalar_coarray[:]

    ! 2. Two allocatable non-coarrays (one array, one scalar)
    integer, allocatable :: array_noncoarray(:)
    integer, allocatable :: scalar_noncoarray

    ! --- CHECK 1: Before allocation ---
    if (allocated(array_coarray)    .or. &
        allocated(scalar_coarray)   .or. &
        allocated(array_noncoarray) .or. &
        allocated(scalar_noncoarray)) then
        error stop "Error: An entity was unexpectedly allocated before ALLOCATE!"
    end if

    ! Allocate all four entities in a SINGLE ALLOCATE statement
    allocate( &
        array_coarray(10)[*], &
        scalar_coarray[*],    &
        array_noncoarray(5),  &
        scalar_noncoarray     &
    )

    ! --- CHECK 2: After allocation ---
    if (.not. allocated(array_coarray)    .or. &
        .not. allocated(scalar_coarray)   .or. &
        .not. allocated(array_noncoarray) .or. &
        .not. allocated(scalar_noncoarray)) then
        error stop "Error: One or more entities failed to allocate properly!"
    end if

    ! Clean up memory
    deallocate(array_coarray, scalar_coarray, array_noncoarray, scalar_noncoarray)

    ! --- CHECK 3: After deallocation ---
    if (allocated(array_coarray)    .or. &
        allocated(scalar_coarray)   .or. &
        allocated(array_noncoarray) .or. &
        allocated(scalar_noncoarray)) then
        error stop "Error: One or more entities remained allocated after DEALLOCATE!"
    end if

end program coarrays_30