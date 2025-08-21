module m_toml_array
    type :: toml_array
        integer :: key
    contains
        procedure :: get
    end type toml_array
contains
    subroutine get(this, n)
        class(toml_array), intent(inout) :: this
        integer, intent(in)              :: n
        this%key = n
    end subroutine get
end module m_toml_array

module derived_types_77_mod
    type :: toml_table
        character(len=:), allocatable :: key
    contains
        procedure :: get
    end type toml_table
contains
    subroutine get(this, key)
        class(toml_table), intent(inout) :: this
        character(len=*), intent(in) :: key
        this%key = key
    end subroutine get
end module derived_types_77_mod

program derived_types_77
    use m_toml_array
    use derived_types_77_mod
    implicit none

    type(toml_table) :: tbl
    type(toml_array) :: arr

    arr%key = 10
    tbl%key = "Hello"
    if (arr%key /= 10) error stop
    if (tbl%key /= "Hello") error stop

    call arr%get(5)
    call tbl%get("World")
    if (arr%key /= 5) error stop
    if (tbl%key /= "World") error stop
end program derived_types_77