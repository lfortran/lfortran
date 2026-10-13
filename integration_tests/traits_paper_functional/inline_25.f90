  function sum{INumeric :: T}(x) result(s)
     type(T), intent(in) :: x(:)
     type(T)             :: s
     s = T(0)
     do i := 1, size(x)
        s = s + x(i)
     end do
  end function sum
