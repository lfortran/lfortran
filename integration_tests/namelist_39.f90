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
    integer :: iexit

    namelist /nl/ name_in_program

    page = ''
    iexit = 0
    write(page, nl)

    do i = 1, size(page)
        if (index(page(i), 'NAME_IN_MODULE') /= 0) then
            print *, '<FAILED> NAMELIST does not respect USE RENAMING'
            iexit = 1
            exit
        else if (index(page(i), 'NAME_IN_PROGRAM') /= 0) then
            print *, '<PASSED> NAMELIST respects USE RENAMING'
            exit
        end if
    end do

    if (i == size(page) + 1) then
        print *, '<ERROR> NAMELIST USE RENAMING test internal error'
        iexit = 2
    end if

    if (iexit /= 0) then
        error stop
    end if
end program namelist_39
