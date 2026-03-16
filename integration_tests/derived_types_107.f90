module derived_types_107_mod1
   type base
   contains
      procedure,nopass::prc1
   end type

   type, extends(base)::child
   contains
      procedure,nopass::prc1=>cproc1
   end type

contains
   integer function prc1()
      prc1=100
   end function
   integer function cproc1()
      cproc1=200
   end function
end module

module derived_types_107_mod2
   type base
   contains
      procedure,nopass::prc1
   end type

   type, extends(base)::child
   contains
      procedure,nopass::prc1=>cproc1
   end type
contains
   integer function prc1()
      prc1=300
   end function
   integer function cproc1()
      cproc1=400
   end function
end module

program derived_types_107
   use derived_types_107_mod1, only:base
   use derived_types_107_mod2, only:child

   type(child)::tc1
   type(base)::tb1
   print *, tb1%prc1()
   print *, tc1%prc1()
   if (tc1%prc1()/=400) error stop "Test failed"
   if (tb1%prc1()/=100) error stop "Test failed"
   print *,'pass'
end program