module submodule_21b_string_mod
  implicit none
  private
  public :: string_t

  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: assignment(=) => assign_character_to_string_t
    procedure, private :: assign_character_to_string_t
    procedure :: file_extension
  end type

  interface
    elemental module subroutine assign_character_to_string_t(lhs, rhs)
      class(string_t), intent(inout) :: lhs
      character(len=*), intent(in) :: rhs
    end subroutine

    elemental module function file_extension(self) result(extension)
      class(string_t), intent(in) :: self
      type(string_t) :: extension
    end function
  end interface

end module submodule_21b_string_mod
