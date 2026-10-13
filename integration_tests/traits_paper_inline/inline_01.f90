  abstract interface :: ICastable
     initial(n)
        integer, intent(in) :: n
     end initial
  end interface ICastable

  abstract interface :: ICalculable
     function saxpy(a,x,y) result(res)
        real, intent(in) :: a, x(:), y(:)
        real             :: res(size(x))
     end function saxpy
  end interface ICalculable

  abstract interface :: IPrintable
     subroutine output()
     end subroutine output
  end interface IPrintable
