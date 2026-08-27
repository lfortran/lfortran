subroutine getwid(se)
   type settings
      integer w
   end type settings
   type(settings), intent(in)  :: se
   character(se % w)           :: stmin(10)

end subroutine getwid