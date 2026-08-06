subroutine next_value(value)
    implicit none
    integer, intent(out) :: value
    integer, allocatable, save :: values(:)

    if (.not. allocated(values)) then
        allocate(values(1))
        values(1) = 0
    end if
    values(1) = values(1) + 1
    value = values(1)
end subroutine

subroutine update_texts(was_allocated)
    implicit none
    logical, intent(out) :: was_allocated
    character(len=:), allocatable, save :: texts(:)

    was_allocated = allocated(texts)
    if (.not. was_allocated) then
        allocate(character(len=3) :: texts(2))
        texts = [character(len=3) :: "abc", "def"]
    else
        if (any(texts /= [character(len=3) :: "abc", "def"])) error stop
        deallocate(texts)
    end if
end subroutine

program allocatable_save_array_01
    implicit none
    integer :: value
    logical :: was_allocated

    call next_value(value)
    if (value /= 1) error stop
    call next_value(value)
    if (value /= 2) error stop

    call update_texts(was_allocated)
    if (was_allocated) error stop
    call update_texts(was_allocated)
    if (.not. was_allocated) error stop
    call update_texts(was_allocated)
    if (was_allocated) error stop
end program
