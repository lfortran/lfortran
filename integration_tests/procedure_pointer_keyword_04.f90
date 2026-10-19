module procedure_pointer_keyword_04_mod
  implicit none

  abstract interface
    subroutine sub_iface(x, y, res)
      integer, intent(in) :: x, y
      integer, intent(out) :: res
    end subroutine
  end interface

contains

  subroutine my_sub(x, y, res)
    integer, intent(in) :: x, y
    integer, intent(out) :: res
    res = 10*x + y
  end subroutine

  subroutine call_dummy(f, res)
    procedure(sub_iface) :: f
    integer, intent(out) :: res
    call f(y=2, x=1, res=res)
  end subroutine

  subroutine call_dummy_pointer(f, res)
    procedure(sub_iface), pointer, intent(in) :: f
    integer, intent(out) :: res
    call f(3, res=res, y=4)
  end subroutine

end module

program procedure_pointer_keyword_04
  use procedure_pointer_keyword_04_mod
  implicit none
  procedure(sub_iface), pointer :: p
  integer :: r

  ! Keyword arguments in calls through procedure variables that are
  ! not components: a local pointer, a dummy procedure and a dummy pointer.
  p => my_sub
  call p(x=5, y=6, res=r)
  if (r /= 56) error stop

  call call_dummy(my_sub, r)
  if (r /= 12) error stop

  call call_dummy_pointer(p, r)
  if (r /= 34) error stop

  print *, "ok"
end program
