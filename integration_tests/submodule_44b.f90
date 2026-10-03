module submodule_44_mod
   implicit none
   type :: mytype
      integer :: x
   end type mytype

   interface create
      module function create1(a) result(res)
         integer, intent(in) :: a
         type(mytype) :: res
      end function create1

      module function create2(a, b) result(res)
         integer, intent(in) :: a, b
         type(mytype) :: res
      end function create2
   end interface create
end module submodule_44_mod

submodule(submodule_44_mod) submodule_44_sub
   implicit none
contains
   module function create1(a) result(res)
      integer, intent(in) :: a
      type(mytype) :: res
      res%x = a
   end function create1

   module function create2(a, b) result(res)
      integer, intent(in) :: a, b
      type(mytype) :: res
      res%x = a + b
   end function create2
end submodule submodule_44_sub

module submodule_44_reexport
   use submodule_44_mod, only: mytype, create
   implicit none
   private
   public :: mytype, create
end module submodule_44_reexport
