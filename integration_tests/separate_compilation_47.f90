program separate_compilation_47
   use separate_compilation_47_b
   use separate_compilation_47_a, only: AbsType, AbsTypeDerived
   implicit none

   class(AbsType), allocatable :: res
   integer, parameter :: expected_value = 123

   res = myfunc()
   if (.not. allocated(res)) error stop "separate_compilation_47: allocation failed"
   select type (res)
   type is (AbsTypeDerived)
      if (res%value /= expected_value) error stop "separate_compilation_47: value mismatch"
   class default
      error stop "separate_compilation_47: unexpected derived type"
   end select
   print *, "ok"
end program separate_compilation_47
