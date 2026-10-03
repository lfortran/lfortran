! Test for https://github.com/lfortran/lfortran/issues/6787
! Abstract type-bound operator was rejected by semantic analysis
! Error was: "add doesn't exist inside abstype type"
module class_99_abstracted
   type, abstract :: abstype
   contains
      private
      procedure(addop), deferred :: add
      generic, public :: operator(+) => add
   end type abstype

   abstract interface
      function addop(a,b) result (r)
         import :: abstype
         class(abstype), intent(in) :: a, b
         class(abstype), allocatable :: r
      end function addop
   end interface
end module class_99_abstracted

module class_99_test
   use class_99_abstracted
end module class_99_test

program class_99
  use class_99_test
  print *, "PASS"
end program class_99
