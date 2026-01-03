subroutine outer(x)

   implicit real(kind=8) (a-h,o-z), integer(kind=4) (i-n)

contains
   
   function inner()
      implicit real(kind=8) (a-h,o-z), integer(kind=4) (i-n)
      inner = 1
   end function inner

end subroutine outer

program implicit_typing_04 
   implicit none
   real(8) :: x
   call outer(x)
end program
