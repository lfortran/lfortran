module functions_59_mod
implicit none

type :: t
contains
   procedure :: get_c_impl
end type

contains

function get_c_impl(this) result(v)
   class(t), intent(in) :: this
   complex :: v

   v = cmplx(1.0, 2.0)
end function get_c_impl

subroutine get_c(this, val)
   class(t), intent(in) :: this
   complex, intent(out) :: val

   val = this%get_c_impl()
end subroutine get_c

end module functions_59_mod


program functions_59
   use functions_59_mod
   implicit none

   type(t) :: x
   complex :: v

   call get_c(x, v)

   if (abs(real(v) - 1.0) > 1e-6 .or. abs(aimag(v) - 2.0) > 1e-6) error stop

end program functions_59
