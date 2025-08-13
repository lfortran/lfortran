program functions_41
   implicit none
   
   call notrecursive_sub
   print *, notrecursive_fun()

contains
   non_recursive subroutine notrecursive_sub
      print *, "This is a non-recursive subroutine"
   end subroutine notrecursive_sub

   non_recursive integer function notrecursive_fun()
      print *, "This is a non-recursive function"
      notrecursive_fun = 1
   end function notrecursive_fun
end program functions_41
