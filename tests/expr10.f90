module m

   contains

      subroutine f(filename)
      character(len=*), intent(in) :: filename
      integer :: u
      open(newunit=u, file=trim(filename))
      end subroutine

end module
