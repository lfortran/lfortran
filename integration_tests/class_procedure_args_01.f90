module class_procedure_args_01

   type,public :: clock
   contains
      procedure,public :: toc => clock_end
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

end module class_procedure_args_01


program main

   use class_procedure_args_01

   procedure(sub), pointer :: toc
   call clk%toc("5")

   toc => sub
   call toc("6")

end program main

