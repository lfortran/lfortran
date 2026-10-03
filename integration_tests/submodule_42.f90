! Test: module procedure implementation of a procedure declared inside
! a named (generic) interface block within a submodule.
module submodule_42_m
  implicit none

  type :: mytype
    integer :: x
  end type

  interface
    elemental module function foo(self) result(res)
      class(mytype), intent(in) :: self
      integer :: res
    end function
  end interface

end module

submodule(submodule_42_m) submodule_42_s
  implicit none

  interface assert_conformable
    elemental module subroutine bar(self, other)
      class(mytype), intent(in) :: self
      type(mytype), intent(in) :: other
    end subroutine
  end interface

contains

  module procedure foo
    res = self%x
  end procedure

  module procedure bar
    continue
  end procedure

end submodule

program submodule_42
  use submodule_42_m
  implicit none
  type(mytype) :: a, b
  a = mytype(42)
  b = mytype(10)
  if (foo(a) /= 42) error stop
  if (foo(b) /= 10) error stop
  print *, "ok"
end program
