module class_procedure_args_01

   type,public :: clock
   contains
      procedure,public :: toc => clock_end
      procedure :: increment
   end type clock

   type(clock),public :: clk

contains

   subroutine clock_end(me, case_str)
      class(clock),intent(inout) :: me
      character(len=*),intent(in) :: case_str
      print *, case_str
      if (case_str /= "5") stop
   end subroutine clock_end

   subroutine sub(case_str)
      character(len=*),intent(in) :: case_str
      print *, case_str
      if (case_str /= "6") error stop
   end subroutine sub

   function increment(me, x) result(res)
      class(clock),intent(inout) :: me
      integer, intent(in) :: x
      integer :: res
      res = x + 1
   end function increment
end module class_procedure_args_01


program main

   use class_procedure_args_01

   procedure(sub), pointer :: toc
   call clk%toc("5")

   toc => sub
   call toc("6")
   if (clk%increment(x=5) /= 6) error stop
end program main

