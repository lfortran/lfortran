module procedure_28_absmod
   implicit none

   type, abstract :: AbsType
   end type AbsType

   abstract interface
      function func_intfc(c) result(res)
         import :: AbsType
         implicit none
         class(AbsType), intent(in)  :: c
         class(*),       allocatable :: res
      end function func_intfc
   end interface

end module procedure_28_absmod


module procedure_28_typemod
   use procedure_28_absmod, only: AbsType, func_intfc
   implicit none

   type, public, extends(AbsType) :: MyType
      integer :: value
   contains
      procedure :: method
   end type MyType

contains

   function method(self) result(res)
      class(MyType), intent(in)  :: self
      class(*),      allocatable :: res
      procedure(func_intfc), pointer :: func

      func => my_function

      if (.not. associated(func)) then
         error stop "Procedure pointer not associated"
      end if

      res = func(self)

      if (.not. allocated(res)) then
         error stop "Result was not allocated"
      end if

   end function method


   function my_function(c) result(res)
      class(AbsType), intent(in) :: c
      class(*), allocatable      :: res

      select type (c)
      type is (MyType)
         allocate(res, source=c%value)
      class default
         error stop "Unsupported type passed to my_function"
      end select

   end function my_function

end module procedure_28_typemod


program procedure_28
   use procedure_28_typemod
   implicit none

   type(MyType) :: obj
   class(*), allocatable :: result

   obj%value = 10

   result = obj%method()

   select type (result)
   type is (integer)
      print *, "Result =", result
      if (result /= 10) then
         error stop "Unexpected result value"
      end if
   class default
      error stop "Unexpected result type"
   end select

end program procedure_28
