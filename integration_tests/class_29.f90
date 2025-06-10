program class_29
    type :: toml_val
        integer :: x
    end type
    type :: toml_table
        integer :: x
        character(len=:), allocatable :: y
        class(toml_val), allocatable :: val
    end type
    class(toml_table), pointer :: temp
    class(toml_table), pointer :: temp2
    class(toml_table), allocatable :: temp3
    type(toml_table), allocatable :: temp4
    type(toml_table), target :: temp_table
    temp_table%x = 42
    temp => temp_table
    nullify(temp2)
    if (associated(temp2) .or. .not. associated(temp)) error stop
    
    if (allocated(temp3)) error stop
    allocate(temp3)
    allocate(temp4)
    if (.not. allocated(temp3)) error stop
    temp3%y = "Hello, World!"
    if (allocated(temp3%val)) error stop
    call destroy(temp3)
    if (allocated(temp3%y)) error stop
    temp3%y = "Hello, World!"
    temp4%y = "Hello, World!"
    if (allocated(temp3%y))  then
        deallocate(temp3%y)
    end if
    if (allocated(temp3%y)) error stop
    if (allocated(temp4%y))  then
        deallocate(temp4%y)
    end if
    if (allocated(temp4%y)) error stop
contains 
    subroutine destroy(shlex)
        class(toml_table), intent(inout) :: shlex
        deallocate(shlex%y)
    end subroutine destroy
end program