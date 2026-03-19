module separate_compilation_47_b
   use separate_compilation_47_a, only: AbsType, AbsTypeDerived
contains
   function myfunc() result(res)
      class(AbsType), allocatable :: res
      allocate(res, source=srcfunc())
   end function myfunc

   function srcfunc() result(obj)
      class(AbsType), allocatable :: obj
      integer, parameter :: expected_value = 123
      allocate(AbsTypeDerived :: obj)
      select type (obj)
      type is (AbsTypeDerived)
         obj%value = expected_value
      class default
      end select
   end function srcfunc
end module separate_compilation_47_b
