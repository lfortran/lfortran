program separate_compilation_20
    use mod_separate_compilation_20

    implicit none

    integer :: key = 5
    call map_open_entry_sc_20( key )

    print *, key
    if (key /= 1) error stop
end program