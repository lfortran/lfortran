module intrinsic_array_m_submodule_56
  implicit none
  private
  public :: intrinsic_array_t

  type :: intrinsic_array_t
    integer, allocatable :: integer_1D(:)
  contains
    procedure :: as_character
  end type

  interface intrinsic_array_t
    pure module function construct(array) result(intrinsic_array)
      class(*), intent(in) :: array(..)
      type(intrinsic_array_t) intrinsic_array
    end function
  end interface

  interface
    pure module function as_character(self) result(s)
      class(intrinsic_array_t), intent(in) :: self
      character(len=:), allocatable :: s
    end function
  end interface
end module intrinsic_array_m_submodule_56
