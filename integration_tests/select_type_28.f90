! Test struct constructor with class member inside select type
! Verifies no double free when constructing a struct from a
! select type associated variable
module select_type_28_type1mod

   type :: Type1
   end type Type1

contains

   function get_Type1() result(res)
      class(*), allocatable :: res
      res = Type1()
   end function get_Type1

end module select_type_28_type1mod

module select_type_28_type2mod

   use select_type_28_type1mod

   type :: Type2
      class(Type1), allocatable :: field
   end type Type2

contains

   function get_Type2() result(res)
      class(*), allocatable :: res
      select type( obj => get_Type1() )
      class is (Type1)
         res = Type2(obj)
      end select
   end function get_Type2

end module select_type_28_type2mod

program select_type_28

   use select_type_28_type2mod

   class(Type2), allocatable :: res

   select type( obj => get_Type2() )
   class is (Type2)
      res = obj
   end select

   print *, "PASS"

end program select_type_28
