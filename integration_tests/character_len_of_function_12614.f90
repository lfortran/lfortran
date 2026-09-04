module character_len_of_function_12614_m
   implicit none
contains

   character(len=28) function myfunc(iarg)
      integer,intent(in),optional :: iarg
      if(present(iarg))then
         myfunc=repeat('@',iarg)
      else
         myfunc='fini '//printdat()
      end if
   contains

      function printdat()
         character(len=len(myfunc)) :: printdat
         printdat='printed'
      end function printdat

   end function myfunc

end module character_len_of_function_12614_m
program character_len_of_function_12614
use character_len_of_function_12614_m, only : myfunc
   write(*,*)myfunc(),myfunc(3)
end program character_len_of_function_12614
