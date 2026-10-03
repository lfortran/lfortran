program coarrays_31
    implicit none

    ! Declare an allocatable array coarray
    integer, allocatable, save :: arr_coarray(:)[:]
    integer :: i

    ! Allocate the coarray using a SOURCE= specifier.
    ! The array dimensions and values are taken from the array constructor.
    ! The explicit [*] defines the co-dimensions.
    allocate(arr_coarray[*], source=[(i * 10, i = 1, 5)])

    ! Verify the allocation state
    if (.not. allocated(arr_coarray)) then
        error stop "Error: arr_coarray was not allocated."
    end if

    ! Verify the inherited size and values
    if (size(arr_coarray) /= 5) then
        error stop "Error: array size does not match SOURCE."
    end if

    if (any(arr_coarray /= [10, 20, 30, 40, 50])) then
        error stop "Error: array values do not match SOURCE."
    end if

end program coarrays_31