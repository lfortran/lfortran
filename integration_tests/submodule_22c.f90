module mod_a
  implicit none
  interface
    pure module function foo(x) result(res)
      class(*), intent(in) :: x
      integer :: res
    end function
  end interface
end module
