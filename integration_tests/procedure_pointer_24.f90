program procedure_pointer_24
  implicit none
  type :: t
    integer :: x
  end type
  type(t) :: obj
  procedure(sub), pointer :: p
  p => f(sub)
  obj%x = 42
  call p(obj)
contains
  subroutine sub(a)
    type(t), intent(in) :: a
    if (a%x /= 42) error stop
    print *, "PASS"
  end subroutine
  function f(fp)
    procedure(sub) :: fp
    procedure(sub), pointer :: f
    f => fp
  end function
end program
