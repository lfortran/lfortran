! Test for https://github.com/lfortran/lfortran/issues/6975
! Segfault upon compilation of select type with deferred function result
module class_100_module

   type, abstract :: InputType
   contains
      procedure(some_method), deferred :: some_method
   end type InputType

   type, abstract :: OutputType
   end type OutputType

   abstract interface
      function some_method(self) result(obj)
         import
         class(InputType), intent(in)  :: self
         class(*),         allocatable :: obj
      end function some_method
   end interface

contains

   function some_func(t) result(res)
      class(InputType), intent(in)  :: t
      class(*),         allocatable :: res
      select type( obj => t%some_method() )
      class is (OutputType)
         res = obj
      end select
   end function some_func

end module class_100_module

program class_100
   use class_100_module
   print *, "PASS"
end program class_100
