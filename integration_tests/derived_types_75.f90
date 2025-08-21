module installer_mod_derived_types_75
    implicit none

    type :: installer_t
        integer :: install_version
    contains
        procedure :: install_header
        procedure :: install
    end type installer_t

    type :: error_t
    end type

contains

    subroutine install_header(self, error)
        class(installer_t), intent(inout) :: self
        type(error_t), allocatable, intent(out) :: error
        if (.true.) then
            call self%install()
        end if
        call self%install()
    end subroutine install_header

    subroutine install(self)
        class(installer_t), intent(inout) :: self
        self%install_version = self%install_version + 927 
    end subroutine install

end module installer_mod_derived_types_75


program derived_types_75
    use installer_mod_derived_types_75
    implicit none

    type(installer_t) :: installer
    type(error_t), allocatable :: error

    installer%install_version = 1
    call installer%install_header(error)
    print *, "Installer version after installation: ", installer%install_version
    if (installer%install_version /= 1855) error stop
end program derived_types_75