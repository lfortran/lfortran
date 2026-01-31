! Test logical array element access and assignment
! This exercises the logical_array_cast ASR pass which handles i1<->i8 conversions
program logical_array_01
    implicit none
    logical :: arr1d(4)
    logical :: arr2d(2, 3)
    logical :: scalar
    integer :: i, j

    ! Test 1: Basic assignment and reading
    arr1d(1) = .true.
    arr1d(2) = .false.
    arr1d(3) = .true.
    arr1d(4) = .false.

    if (.not. arr1d(1)) error stop "Test 1a failed"
    if (arr1d(2)) error stop "Test 1b failed"
    if (.not. arr1d(3)) error stop "Test 1c failed"
    if (arr1d(4)) error stop "Test 1d failed"

    ! Test 2: Negation of array elements
    arr1d(2) = .not. arr1d(2)
    arr1d(4) = .not. arr1d(4)
    if (.not. arr1d(2)) error stop "Test 2a failed"
    if (.not. arr1d(4)) error stop "Test 2b failed"

    ! Test 3: Logical operations between array elements
    scalar = arr1d(1) .and. arr1d(3)
    if (.not. scalar) error stop "Test 3a failed"

    scalar = arr1d(1) .or. arr1d(2)
    if (.not. scalar) error stop "Test 3b failed"

    ! Test 4: Assignment between array elements
    arr1d(1) = .false.
    arr1d(2) = arr1d(3)  ! Copy .true. from arr1d(3)
    if (.not. arr1d(2)) error stop "Test 4 failed"

    ! Test 5: 2D array
    do i = 1, 2
        do j = 1, 3
            arr2d(i, j) = (mod(i + j, 2) == 0)
        end do
    end do
    ! arr2d should be (even sum = true):
    ! (1,1)=T (1,2)=F (1,3)=T
    ! (2,1)=F (2,2)=T (2,3)=F
    if (.not. arr2d(1, 1)) error stop "Test 5a failed"
    if (arr2d(1, 2)) error stop "Test 5b failed"
    if (.not. arr2d(1, 3)) error stop "Test 5c failed"
    if (arr2d(2, 1)) error stop "Test 5d failed"
    if (.not. arr2d(2, 2)) error stop "Test 5e failed"
    if (arr2d(2, 3)) error stop "Test 5f failed"

    ! Test 6: Array element in if condition (arr2d(1,1) is .true.)
    if (arr2d(1, 1)) then
        scalar = .true.
    else
        error stop "Test 6 failed"
    end if

    ! Test 7: Chained operations (arr2d(2,2) is .true.)
    arr1d(1) = .not. (.not. arr2d(2, 2))
    if (.not. arr1d(1)) error stop "Test 7 failed"

    ! Test 8: Passing array element to function
    arr1d(1) = .true.
    arr1d(2) = .false.
    if (.not. negate_logical(arr1d(2))) error stop "Test 8a failed"
    if (negate_logical(arr1d(1))) error stop "Test 8b failed"

    ! Test 9: Comparison between array elements (eqv/neqv)
    arr1d(1) = .true.
    arr1d(2) = .true.
    arr1d(3) = .false.
    if (.not. (arr1d(1) .eqv. arr1d(2))) error stop "Test 9a failed"
    if (arr1d(1) .eqv. arr1d(3)) error stop "Test 9b failed"
    if (.not. (arr1d(1) .neqv. arr1d(3))) error stop "Test 9c failed"

    print *, "All logical array tests passed!"

contains

    pure logical function negate_logical(val)
        logical, intent(in) :: val
        negate_logical = .not. val
    end function negate_logical

end program logical_array_01
