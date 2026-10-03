module submodule_38_mod
  implicit none

  type :: base_t
  end type

  type, extends(base_t) :: child_t
    integer :: k_
  contains
    generic :: operator(.x.) => multiply
    procedure, private :: multiply
  end type

  interface
    module function multiply(self, vec) result(r)
      class(child_t), intent(in) :: self
      double precision, intent(in) :: vec(:)
      double precision, allocatable :: r(:)
    end function
  end interface
end module
