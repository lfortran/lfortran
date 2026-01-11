program implied_do_loops16
    implicit none
    character(len=*), parameter :: scr = "src/"
    character(len=:), allocatable :: expected(:)

    type :: string_t
        character(len=:), allocatable :: s
    end type string_t

    type(string_t), allocatable :: file_names(:)

    integer :: j

    allocate(character(len=5) :: expected(2))
    expected = ["mod1 ", "mod2 "]

    allocate(file_names(2))
    file_names(1)%s = "src/mod1"
    file_names(2)%s = "src/mod2"

    if (.not. allocated(expected)) error stop "ERROR: expected not allocated"
    if (.not. allocated(file_names)) error stop "ERROR: file_names not allocated"

    if (size(expected) /= size(file_names)) error stop &
        "ERROR: size mismatch between expected and file_names"

    do j = 1, size(expected)
        if (len_trim(expected(j)) == 0) error stop "ERROR: empty expected entry"
        if (.not. allocated(file_names(j)%s)) error stop "ERROR: unallocated file name"
    end do

    write(*,'("EXPECTED: ",*(g0:,","))') &
        (scr // trim(expected(j)), j=1, size(expected))

    write(*,'("FOUND:    ",*(g0:,","))') &
        (trim(file_names(j)%s), j=1, size(file_names))

    do j = 1, size(expected)
        if (scr // trim(expected(j)) /= trim(file_names(j)%s)) then
            error stop "ERROR: EXPECTED and FOUND differ"
        end if
    end do

end program
