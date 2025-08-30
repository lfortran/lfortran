module submodule_13_mod
  implicit none

  private
  public :: string_t
  public :: operator(.separatedBy.)

  type string_t
    private
    character(len=:), allocatable :: string_
  contains
    procedure :: bracket
  end type string_t

  interface string_t
    elemental module function from_default_integer(i) result(string)
      implicit none
      integer, intent(in) :: i
      type(string_t) :: string
    end function from_default_integer
  end interface

  interface
    elemental module function bracket(self, opening, closing) result(bracketed_self)
      implicit none
      class(string_t), intent(in) :: self
      character(len=*), intent(in), optional :: opening, closing
      type(string_t) :: bracketed_self
    end function bracket
  end interface

end module submodule_13_mod


program submodule_13
  use submodule_13_mod, only : string_t
  implicit none

contains

  pure function markdown_table(row_header, column_header, body_cells, side_borders) result(lines)
    integer, parameter :: first_body_row = 3
    type(string_t), intent(in) :: row_header(first_body_row:), column_header(:), body_cells(first_body_row:,:)
    logical, intent(in) :: side_borders
    character(len=1), parameter :: column_separator = "|"
    integer, parameter :: num_rule_lines = 1
    type(string_t) :: lines(size(body_cells,1) + num_rule_lines)

    if (side_borders) lines = lines%bracket(column_separator)
  end function markdown_table

end program submodule_13
