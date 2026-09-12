module character_len_of_function_12614_m
   implicit none
contains
   character(len=28) function myfunc()
   contains
      function printdat()
         character(len=len(myfunc)) :: printdat
         printdat = 'printed'
      end function printdat
   end function myfunc
end module character_len_of_function_12614_m
