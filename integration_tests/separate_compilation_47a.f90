module separate_compilation_47a_absmod
   type, abstract :: AbsType
      integer :: x = 0
   end type AbsType

   type, extends(AbsType) :: ConcreteType
      integer :: y = 0
   end type ConcreteType
end module separate_compilation_47a_absmod

module separate_compilation_47a_mymod
   use separate_compilation_47a_absmod
contains
   function myfunc() result(res)
      class(AbsType), allocatable :: res
      allocate(res, source=srcfunc())
   end function myfunc
   function srcfunc() result(obj)
      class(AbsType), allocatable :: obj
      allocate(ConcreteType :: obj)
      obj%x = 42
      select type(obj)
      type is (ConcreteType)
         obj%y = 84
      end select
   end function srcfunc
end module separate_compilation_47a_mymod
