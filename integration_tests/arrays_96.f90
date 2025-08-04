program arrays_96
   integer :: arr(2)
   arr(1) = 1
   arr(2) = 121
   call ff(arr)
   contains

   subroutine ff(s) 
       integer, dimension(0:), intent(inout) :: s ! TODO: Fix case where intent is `unspecified`
       print *, s(0)
       if(s(0) /= 1) error stop
       print *, s(1)
       if(s(1) /= 121) error stop
   end subroutine
end program
