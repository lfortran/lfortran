program separate_compilation_47
   use separate_compilation_47a_mymod
   implicit none
   class(AbsType), allocatable :: val
   val = myfunc()
   if (val%x /= 42) error stop
   select type(val)
   type is (ConcreteType)
      if (val%y /= 84) error stop
   class default
      error stop
   end select
   print *, "ok"
end program separate_compilation_47
