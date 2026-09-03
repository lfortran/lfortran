module m_generic_submod
 implicit none
 interface f
  module function f() result(y)
   integer :: y
  end function f
 end interface
end module m_generic_submod

submodule (m_generic_submod) sm
contains
 module function f() result(y)
  integer :: y
  y = 4
 end function f
end submodule sm

program p
 use m_generic_submod
 implicit none
 if (f() /= 4) error stop
 print *, "PASS"
end program p