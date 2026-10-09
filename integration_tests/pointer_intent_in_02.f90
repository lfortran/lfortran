module pointer_intent_in_02_m
  implicit none
  type :: t
    integer :: i = 0
    integer, pointer :: p => null()
  contains
    procedure :: point_at
  end type
  interface set_ptr
    module procedure set_int_ptr
  end interface
contains

  subroutine set_int_ptr(w, tgt)
    integer, pointer, intent(inout) :: w
    integer, target, intent(in) :: tgt
    w => tgt
  end subroutine

  subroutine point_at(self, w, tgt)
    class(t), intent(in) :: self
    integer, pointer :: w
    integer, target, intent(in) :: tgt
    w => tgt
  end subroutine

  integer function get_int(w)
    integer, pointer, intent(in) :: w
    get_int = w
  end function

  integer function get_i(w)
    class(t), pointer, intent(in) :: w
    get_i = w%i
  end function

end module

program pointer_intent_in_02
  ! Actual arguments that a pointer dummy accepts: a pointer for any
  ! intent, and a pointer or a valid target for INTENT(IN).
  use pointer_intent_in_02_m
  implicit none
  integer, target :: a = 3, b = 5
  integer, pointer :: ip => null()
  type(t), target :: x
  type(t) :: y
  class(t), allocatable, target :: c

  call set_ptr(ip, a)
  if (get_int(ip) /= 3) error stop
  call y%point_at(ip, b)
  if (get_int(ip) /= 5) error stop
  call set_ptr(y%p, a)
  if (get_int(y%p) /= 3) error stop
  if (get_int(a) /= 3) error stop

  x%i = 4
  if (get_i(x) /= 4) error stop
  allocate(c)
  c%i = 9
  if (get_i(c) /= 9) error stop

  associate (q => x)
    if (get_i(q) /= 4) error stop
  end associate
  select type (s => c)
  type is (t)
    if (get_i(s) /= 9) error stop
  end select
  print *, "ok"
end program
