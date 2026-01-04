module derived_types_91_mod
    implicit none
    private
    public :: ansi_code, toml_terminal

    type :: ansi_code
        integer :: style = -1
    contains
        procedure :: is_valid
    end type ansi_code

    type :: toml_terminal
        type(ansi_code) :: reset = ansi_code()
    contains
        procedure :: check
    end type toml_terminal

contains

    logical function is_valid(self)
        class(ansi_code), intent(in) :: self
        is_valid = (self%style == -1)
    end function is_valid


    subroutine check(self)
        class(toml_terminal), intent(in) :: self

        if (.not. self%reset%is_valid()) then
            error stop "ERROR: toml_terminal.reset is invalid"
        end if
    end subroutine check

end module derived_types_91_mod


module derived_types_91_mod_2
    use derived_types_91_mod, only: toml_terminal
    implicit none
    private
    public :: toml_parser_config

    type :: toml_parser_config
        type(toml_terminal) :: color = toml_terminal()
    contains
        procedure :: check
    end type toml_parser_config

contains

    subroutine check(self)
        class(toml_parser_config), intent(in) :: self

        call self%color%check()
    end subroutine check

end module derived_types_91_mod_2


program derived_types_91
    use derived_types_91_mod_2, only: toml_parser_config
    implicit none

    type(toml_parser_config) :: config

    call config%check()

end program derived_types_91
