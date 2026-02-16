module submodule_20b_mod
  implicit none
  type :: t
  contains
    procedure :: foo
  end type
  interface
    module subroutine foo(self)
      class(t), intent(in) :: self
    end subroutine
  end interface
end module
