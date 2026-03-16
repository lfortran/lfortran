module nested_24_mod
  abstract interface
    function iface(x)
      integer, intent(in) :: x
      integer :: iface
    end function
  end interface
contains
  function f(x)
    integer, intent(in) :: x
    integer :: f
    f = x + 10
  end function
end module

program nested_24
  use nested_24_mod
  implicit none
  procedure(iface), pointer :: p => f
  call sub()
contains
  subroutine sub()
    if (p(1) /= 11) error stop
    if (p(5) /= 15) error stop
    print *, "ok"
  end subroutine
end program
