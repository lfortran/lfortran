  abstract interface :: ISum
     function sum{integer | real(real64) :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
     end function sum
  end interface ISum
