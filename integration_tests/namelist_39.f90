module namelist_39_module
    implicit none
    integer, save :: name_in_module = 123
end module namelist_39_module

program namelist_39
    use namelist_39_module, &
        & name_in_program => name_in_module
    implicit none

    character(len=80) :: page(4)
    integer :: i
    logical :: found

    namelist /nl/ name_in_program

    page = ''
    found = .false.
    write(page, nl)

    do i = 1, size(page)
        if (index(page(i), 'NAME_IN_MODULE') /= 0) then
            error stop 'NAMELIST does not respect USE renaming'
        else if (index(page(i), 'NAME_IN_PROGRAM') /= 0) then
            found = .true.
        end if
    end do

    if (.not. found) error stop 'NAMELIST output did not contain NAME_IN_PROGRAM'
end program namelist_39
