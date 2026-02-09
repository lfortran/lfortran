module class_105_mod
   implicit none

   type :: AbsType
      integer :: value
   end type AbsType

   type :: MyType
      class(AbsType), allocatable :: arr(:)
   end type MyType

contains

   function tester() result(obj)
      class(MyType), allocatable :: obj
      obj = MyType()
      allocate(obj%arr(1))
      obj%arr(1)%value = 42
      print *, "Inside tester: obj%arr(1)%value =", obj%arr(1)%value
   end function tester

end module class_105_mod

program class_105
   use class_105_mod
   implicit none

   class(MyType), allocatable :: result_obj

   ! Call tester function
   allocate(result_obj)
   result_obj = tester()

   ! Check if obj is assigned correctly
   if (allocated(result_obj)) then
      print *, "SUCCESS: obj is allocated correctly"
      select type (result_obj)
       class is (MyType)
         print *, "SUCCESS: obj is of type MyType"
         print *, "obj%arr(1)%value =", result_obj%arr(1)%value
       class default
         error stop "FAILURE: obj is not of type MyType"
      end select
   else
      error stop "FAILURE: obj is not allocated"
   end if

end program class_105
