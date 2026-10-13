  abstract interface :: ISum
     function sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
     end function sum
  end interface ISum
