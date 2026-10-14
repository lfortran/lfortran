  abstract interface, extends(ICastable + IPrintable) :: IAdmissible
     subroutine input(self,filename)
        type(itself), intent(in), pass :: self
        character(*), intent(in) :: filename
     end subroutine input
  end interface IAdmissible
