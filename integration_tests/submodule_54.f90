module submodule_54_mod
  implicit none
  type :: t
    procedure(ifn), pointer, nopass :: p1 => null()
    procedure(ifn), pointer, nopass :: p2 => null()
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
    module subroutine setup(c)
      type(t), intent(inout) :: c
    end subroutine
  end interface
end module

submodule(submodule_54_mod) submodule_54_impl
contains
  module subroutine impl(a, b)
    real, intent(in)  :: a(:)
    real, intent(out) :: b(:)
    b = a
  end subroutine
  subroutine impl_reshape(a, b)
    real, intent(in)  :: a(:)
    real, intent(out) :: b(:)
    real :: tmp(size(a))
    tmp = reshape(a, [size(a)])
    b = tmp
  end subroutine
  module subroutine setup(c)
    type(t), intent(inout) :: c
    c%p1 => impl
    c%p2 => impl_reshape
  end subroutine
end submodule

program submodule_54
  use submodule_54_mod
  implicit none
  type(t) :: c
  real :: a(3), b(3)
  a = 1.0
  call setup(c)
  b = 0.0
  call c%p1(a, b)
  if (any(b /= a)) error stop "p1"
  b = 0.0
  call c%p2(a, b)
  if (any(b /= a)) error stop "p2"
end program
