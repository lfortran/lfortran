module procedure_12_mod

contains

   recursive subroutine sub(int, X)
      integer, intent(in) :: int
      integer, intent(inout) :: X(:)
      procedure(sub), pointer :: proc 
      proc => sub
      
      if (int <= 5) X(int) = int
      if (int > 5) return
      if (mod(int, 2) /= 0) then
         print *, "Odd: ", int
         call proc(int + 1, X)
      else
         proc => alt_sub
         call proc(int, X)
      end if
   end subroutine sub

   subroutine alt_sub(int, X)
      integer, intent(in) :: int
      integer, intent(inout) :: X(:)
      if (int <= 5) X(int) = int
      print *, "Even: ", int
      call sub(int + 1, X)
   end subroutine alt_sub

end module procedure_12_mod

program procedure_12
   use procedure_12_mod
   integer :: X(5) = [0, 0, 0, 0, 0]
   call sub(1, X)
   print *, X
   if(X(1) /= 1 .or. X(2) /= 2 .or. X(3) /= 3 .or. X(4) /= 4 .or. X(5) /= 5) error stop
end program procedure_12