program separate_compilation_16
    use mod_separate_compilation_16

    implicit none

    integer :: key = 5
    call map_open_entry( key )

    print *, key
    if (key /= 1) error stop
end program