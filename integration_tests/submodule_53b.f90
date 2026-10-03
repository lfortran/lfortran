module submodule_53_mod
  implicit none
  type :: t
    procedure(ifn), pointer, nopass :: p => null()
  end type
  interface
    module subroutine ifn(a, b)
      real, intent(in)  :: a(:)
      real, intent(out) :: b(:)
    end subroutine
    module subroutine impl(a, b)
      real, intent(in)  :: a(:)
      real, intent(out) :: b(:)
    end subroutine
  end interface
end module

submodule(submodule_53_mod) submodule_53_impl
contains
  module subroutine impl(a, b)
    real, intent(in)  :: a(:)
    real, intent(out) :: b(:)
    b = a
  end subroutine
end submodule
