module separate_compilation_02_module
contains
   subroutine m_sub
      print *, "m_sub"
   end subroutine m_sub
end module separate_compilation_02_module

subroutine sub
   print *, "sub"
end subroutine sub

integer function fn()
   use separate_compilation_02_module
   call m_sub
   fn = 19
end function fn
