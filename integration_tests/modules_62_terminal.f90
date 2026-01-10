module modules_62_terminal
  implicit none
  private

  public :: toml_terminal, ansi_code

  ! Nested type
  type :: ansi_code
    integer :: style = -1
    integer :: bg = -1
    integer :: fg = -1
  end type ansi_code

  ! Main type with multiple ansi_code components and default init
  type :: toml_terminal
    type(ansi_code) :: reset = ansi_code()
    type(ansi_code) :: bold = ansi_code()
  end type toml_terminal

  interface toml_terminal
    module procedure :: new_terminal
  end interface toml_terminal

contains

  pure function new_terminal(use_color) result(new)
    logical, intent(in) :: use_color
    type(toml_terminal) :: new
    if (use_color) then
      new%reset = ansi_code(0, -1, -1)
      new%bold = ansi_code(1, -1, -1)
    end if
  end function new_terminal

end module modules_62_terminal
