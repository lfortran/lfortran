  abstract interface :: ICastable
     initial(n)
        integer, intent(in) :: n
     end initial
  end interface ICastable

  abstract interface :: ICalculable
     function saxpy(self,a,x,y) result(res)
        type(itself), intent(in), pass :: self
        real, intent(in) :: a, x(:), y(:)
        real             :: res(size(x))
     end function saxpy
  end interface ICalculable

  abstract interface :: IPrintable
     subroutine output(self)
        type(itself), intent(in), pass :: self
     end subroutine output
  end interface IPrintable
