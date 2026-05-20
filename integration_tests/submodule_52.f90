module submodule_52_m
  implicit none
  interface
    module subroutine foo() bind(C)
    end subroutine

    module integer function bar(x) bind(C, name="bar_c")
      integer, intent(in) :: x
    end function
  end interface
end module

submodule(submodule_52_m) submodule_52_m_sub
  implicit none
contains
  module procedure foo
    print *, "foo called"
  end procedure

  module procedure bar
    bar = x + 1
  end procedure
end submodule

program submodule_52
  use submodule_52_m
  implicit none
  integer :: res

  call foo()

  res = bar(41)
  print *, res
  if (res /= 42) error stop
end program
