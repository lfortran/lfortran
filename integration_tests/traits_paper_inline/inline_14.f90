  abstract interface :: ISum
     function sum{INumeric :: T}(self,x) result(s)
        type(itself), intent(in), pass :: self
        type(T), intent(in) :: x(:)
        type(T)             :: s
     end function sum
  end interface ISum
