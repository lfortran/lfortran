program class_29
    type :: toml_table
        integer :: x
    end type
    class(toml_table), pointer :: temp
    class(toml_table), pointer :: temp2
    type(toml_table), target :: temp_table
    temp_table%x = 42
    temp => temp_table
    print *, associated(temp)
    print *, associated(temp2)
    if (associated(temp2) .or. .not. associated(temp)) error stop
end program