program class_29
    type :: toml_table
        integer :: x
    end type
    class(toml_table), pointer :: temp
    class(toml_table), pointer :: temp2
    class(toml_table), allocatable :: temp3
    type(toml_table), target :: temp_table
    temp_table%x = 42
    temp => temp_table
    nullify(temp2)
    if (associated(temp2) .or. .not. associated(temp)) error stop
    
    if (allocated(temp3)) error stop
    allocate(temp3)
    if (.not. allocated(temp3)) error stop
end program