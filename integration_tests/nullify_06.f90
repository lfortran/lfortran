program nullify_class
    type :: toml_table
        integer :: x
    end type
    class(toml_table), pointer :: temp
    nullify(temp)
end program
