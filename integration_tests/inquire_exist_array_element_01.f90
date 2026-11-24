program inquire_exist_array_element_01
    implicit none
    logical :: exists(2)
    character(len=32) :: filenames(2)

    filenames = 'no_such_inquire_array_element'
    exists = .true.
    inquire(file=filenames(1), exist=exists(1))
    inquire(file=filenames(2), exist=exists(2))
    if (exists(1) .or. exists(2)) error stop
    print *, exists
end program inquire_exist_array_element_01
