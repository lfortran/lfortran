module tomlf_build_array
    implicit none
    private

    public :: get_value

    !> Getter functions to manipulate TOML arrays
    interface get_value
       module procedure :: get_elem_table
       module procedure :: get_elem_value_string
    end interface get_value


 contains


 subroutine get_elem_table(array, pos, ptr, stat)

    !> Instance of the TOML array
    integer, intent(inout) :: array

    !> Position in the array
    integer, intent(in) :: pos

    !> Pointer to child table
    integer, pointer, intent(out) :: ptr

    !> Status of operation
    logical, intent(out), optional :: stat

    nullify(ptr)

    stat = .true.

 end subroutine get_elem_table


 !> Retrieve TOML value as deferred-length character
 subroutine get_elem_value_string(array, pos, ptr, def, stat)

    !> Instance of the TOML array
    logical, intent(inout) :: array

    !> Position in the array
    integer, intent(in) :: pos

    !> Pointer to child table
    integer, pointer, intent(out) :: ptr

    !> Status of operation
    logical, intent(out), optional :: stat

    integer, intent(out), optional :: def

    nullify(ptr)

    stat = .true.

 end subroutine get_elem_value_string


 end module tomlf_build_array


 program a_foo
    use tomlf_build_array, only: get_value
    implicit none

    integer :: a = 10, pos
    integer, pointer :: ptr
    logical :: f = .false.

    call get_value(a, pos, ptr, stat=f)
    call get_value(f, pos, ptr, stat=f)

 end program a_foo
