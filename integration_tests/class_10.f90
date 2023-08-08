module class_10_module
    implicit none
    private

    public :: toml_vector, toml_value, toml_node

    type, abstract :: toml_value

        character(len=:), allocatable :: key

    contains
        procedure :: match_key

    end type toml_value

    type :: toml_node

       !> TOML value payload
       class(toml_value), allocatable :: val

    end type toml_node


    !> Stores TOML values in a list of pointers
    type :: toml_vector

       !> Current number of stored TOML values
       integer :: n = 0

       !> List of TOML values
       type(toml_node), allocatable :: lst(:)

       contains

       procedure :: delete

    end type toml_vector

 contains

    !> Compare raw key of TOML value to input key
    pure function match_key(self, key) result(match)

        !> TOML value instance.
        class(toml_value), intent(in) :: self

        !> TOML raw key to compare to
        character(len=*), intent(in) :: key

        logical :: match

    end function match_key

    !> Delete TOML value at a given key
    subroutine delete(self, key)

        !> Instance of the structure
        class(toml_vector), intent(inout), target :: self

        !> Key to the TOML value
        character(len=*), intent(in) :: key

        integer :: i

            if (self%lst(i)%val%match_key(key)) then
            end if

    end subroutine delete

end module class_10_module

program class_10_program
 use class_10_module
 implicit none
    print *, "ok"
end program class_10_program
