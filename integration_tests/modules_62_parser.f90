module modules_62_parser
  use modules_62_terminal, only: toml_terminal
  implicit none
  private

  public :: parser_config, parse

  ! Type with toml_terminal as component WITH DEFAULT INITIALIZATION
  ! This triggers the bug: the default toml_terminal() constructor contains
  ! references to ansi_code type which isn't explicitly imported
  type :: parser_config
    type(toml_terminal) :: color = toml_terminal()
    integer :: level = 0
  end type parser_config

contains

  subroutine parse(config, val)
    type(parser_config), intent(in) :: config
    integer, intent(out) :: val
    val = config%level + config%color%reset%style
  end subroutine parse

end module modules_62_parser
