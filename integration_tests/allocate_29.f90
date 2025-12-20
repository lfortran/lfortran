program allocate_29
    implicit none

    character(len=:), allocatable :: names(:)

    call list_examples(names)

    if (.not. allocated(names)) then
        error stop "ERROR: names is not allocated"
    end if

    if (size(names) /= 0) then
        error stop "ERROR: size(names) is not 0"
    end if

    if (len(names) /= 1) then
        error stop "ERROR: len(names) is not 1"
    end if

    print *, size(names)

contains

    subroutine list_examples(names)
        character(len=:), allocatable, intent(out) :: names(:)
        integer, parameter :: entry_len = 256
        integer, parameter :: initial_capacity = 64
        character(len=entry_len), allocatable :: entries(:)
        integer :: capacity

        capacity = initial_capacity
        call ensure_entry_capacity(entries, entry_len, capacity)

        allocate(character(len=1) :: names(0))
    end subroutine list_examples

    subroutine ensure_entry_capacity(buffer, string_len, new_capacity)
        integer, intent(in) :: string_len
        integer, intent(in) :: new_capacity
        character(len=string_len), allocatable, intent(inout) :: buffer(:)
        character(len=string_len), allocatable :: tmp(:)

        if (new_capacity <= 0) then
            allocate(character(len=string_len) :: tmp(1))
        else
            allocate(character(len=string_len) :: tmp(new_capacity))
        end if

        call move_alloc(tmp, buffer)
    end subroutine ensure_entry_capacity

end program allocate_29
