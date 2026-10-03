module submodule_47_parent
  implicit none
  interface
    module subroutine init_sub()
    end subroutine
  end interface
end module

submodule(submodule_47_parent) submodule_47_child
  implicit none
contains
  module procedure init_sub
    use iso_c_binding, only: c_int
    integer(c_int) :: x
    x = 42_c_int
    if (x /= 42) error stop
    print *, x
  end procedure
end submodule

program submodule_47
  use submodule_47_parent
  implicit none
  call init_sub()
end program
