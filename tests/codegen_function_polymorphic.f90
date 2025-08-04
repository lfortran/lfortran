module codegen_function_polymorphic
   
   type, abstract :: AbsType
   end type AbsType

contains
   
   function my_func() result(obj)
      class(AbsType), allocatable :: obj
   end function my_func

end module codegen_function_polymorphic