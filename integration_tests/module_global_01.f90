program module_global_01
  implicit none
  interface
    subroutine type_subroutine(ain, aout)
      integer, intent(in) :: ain
      integer, intent(out) :: aout
    end subroutine type_subroutine
    subroutine no_type_subroutine(ain, aout)
      integer, intent(in) :: ain
      integer, intent(out) :: aout
    end subroutine no_type_subroutine
  end interface
  integer :: a, b
  call no_type_subroutine(5, a)
  call type_subroutine(5, b)
  if (a /= 105) error stop
  if (b /= 205) error stop
end program module_global_01
