module fpm_versioning
implicit none

    type :: version_t
        integer, allocatable :: num(:)
    contains
        procedure :: to_string
    end type version_t

    interface char
        module procedure :: as_string
    end interface char

contains

    subroutine to_string(self, string)
        class(version_t), intent(in) :: self
        character(len=:), allocatable, intent(out) :: string
    end subroutine to_string

    function as_string(self) result(string)
        class(version_t), intent(in) :: self
        character(len=:), allocatable :: string

        call self%to_string(string)

    end function as_string

end module fpm_versioning
