program optional_string_arg_02
    implicit none
    character(4), allocatable :: words(:)
    integer :: calls

    calls = 0
    call set_words()
    if (calls /= 1) error stop
    call set_words(words)
    if (calls /= 2) error stop
    if (.not. allocated(words)) error stop
    if (size(words) /= 2) error stop
    if (len(words) /= 4) error stop
    if (words(1) /= 'abcd' .or. words(2) /= 'efgh') error stop
    call set_words()
    if (calls /= 3) error stop
    if (words(1) /= 'abcd' .or. words(2) /= 'efgh') error stop

contains

    subroutine set_words(c)
        character(*), allocatable, optional, intent(out) :: c(:)
        calls = calls + 1
        if (present(c)) then
            if (len(c) /= 4) error stop
            allocate(c(2))
            c(1) = 'abcd'
            c(2) = 'efgh'
        else
            if (calls /= 1 .and. calls /= 3) error stop
        end if
    end subroutine

end program
