module module_52_tomlf_de
    use module_52_tomlf_error, only : toml_error
    use module_52_tomlf_type, only : toml_table
    implicit none


    interface toml_parse
        module procedure :: toml_parse_unit
        module procedure :: toml_parse_string
    end interface toml_parse

contains

subroutine toml_parse_unit(table, unit, error)
    type(toml_table), allocatable, intent(out) :: table
    integer, intent(in) :: unit
    type(toml_error), allocatable, intent(out), optional :: error

end subroutine toml_parse_unit

subroutine toml_parse_string(table, conf, error)
    type(toml_table), allocatable, intent(out) :: table
    character(len=*), intent(in), target :: conf
    type(toml_error), allocatable, intent(out), optional :: error

end subroutine toml_parse_string


end module module_52_tomlf_de
