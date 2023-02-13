module fpm_manifest_executable_modules_30
    implicit none

    public :: executable_config

    type :: executable_config
        character(len=:), allocatable :: name
        character(len=:), allocatable :: source_dir
        character(len=:), allocatable :: main
    end type executable_config
contains
    subroutine default_executable(self, name)
        type(executable_config), intent(out) :: self
        character(len=*), intent(inout) :: name

        self%name = name
        self%source_dir = "app"
        self%main = "main.f90"
    end subroutine default_executable
end module fpm_manifest_executable_modules_30
