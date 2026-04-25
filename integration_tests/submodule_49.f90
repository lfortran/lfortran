module submodule_49_mod
  implicit none
  interface
    module subroutine set_ptr()
    end subroutine
  end interface
end module

submodule(submodule_49_mod) submodule_49_sub
  use iso_c_binding, only: c_ptr, c_f_pointer, c_loc
  implicit none
  integer, pointer :: p(:)
contains
  module procedure set_ptr
    integer, target, save :: s(4)
    type(c_ptr) :: cptr
    s = 42
    cptr = c_loc(s(1))
    call c_f_pointer(cptr, p, [4])
    if (p(1) /= 42) error stop
    if (p(4) /= 42) error stop
    print *, p(1)
  end procedure
end submodule

program submodule_49_main
  use submodule_49_mod
  implicit none
  call set_ptr()
end program
