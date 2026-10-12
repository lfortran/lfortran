  abstract interface, extends(ICastable + IPrintable) :: IAdmissible
     subroutine input(filename)
        character(*), intent(in) :: filename
     end subroutine input
  end interface IAdmissible
