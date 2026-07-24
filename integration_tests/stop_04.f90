module stop_04_mod
   implicit none
   integer :: eval_count = 0

   type :: tester
   contains
      procedure, nopass :: test
   end type tester

contains

   function test() result(retcode)
      integer :: retcode
      eval_count = eval_count + 1
      print *, 'Setting return code!'
      if (eval_count == 1) then
         retcode = 0
      else
         ! The exit code expression must be evaluated exactly once
         retcode = 7
      end if
   end function test

end module stop_04_mod

program stop_04
   use stop_04_mod
   implicit none
   type(tester) :: obj
   call exit(obj%test())
end program stop_04
