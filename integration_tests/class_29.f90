program class_29
    type :: toml_table
        integer :: x
        character(len=:), allocatable :: y
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
    temp3%y = "Hello, World!"
    call destroy(temp3)
    if (allocated(temp3%y)) error stop
contains 
    subroutine destroy(shlex)
        class(toml_table), intent(inout) :: shlex
        deallocate(shlex%y)
    end subroutine destroy
end program