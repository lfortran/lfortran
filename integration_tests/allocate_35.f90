! Test for realloc-lhs-arrays with empty array constructors
! This tests that assigning an empty array to an allocatable array
! correctly allocates the array with size 0 (still considered allocated)
program allocate_35
    implicit none
    logical, allocatable :: subtally(:)
    integer, allocatable :: arr(:)

    ! First assignment allocates with size 3
    subtally = [.true., .false., .true.]
    if (size(subtally) /= 3) error stop
    if (.not. allocated(subtally)) error stop
    if (subtally(1) .neqv. .true.) error stop
    if (subtally(2) .neqv. .false.) error stop
    if (subtally(3) .neqv. .true.) error stop

    ! Second assignment reallocates to size 0
    ! This should still keep the array allocated (Fortran semantics)
    subtally = [logical::]
    if (size(subtally) /= 0) error stop
    if (.not. allocated(subtally)) error stop

    ! Test with integer array as well
    arr = [1, 2, 3, 4, 5]
    if (size(arr) /= 5) error stop
    if (.not. allocated(arr)) error stop

    ! Reallocate to empty array
    arr = [integer::]
    if (size(arr) /= 0) error stop
    if (.not. allocated(arr)) error stop

    ! Reallocate back to non-empty
    arr = [10, 20]
    if (size(arr) /= 2) error stop
    if (arr(1) /= 10) error stop
    if (arr(2) /= 20) error stop

    print *, "All tests passed!"
end program allocate_35
