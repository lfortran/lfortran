module submodule_21a
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  contains
    generic :: assignment(=) => assign_char
    procedure, private :: assign_char
  end type

  interface
    elemental module subroutine assign_char(lhs, rhs)
      class(string_t), intent(inout) :: lhs
      character(len=*), intent(in) :: rhs
    end subroutine

    module subroutine do_stuff(x)
      type(string_t), intent(inout) :: x
    end subroutine
  end interface

end module
