! A program variable used in internal procedures whose type depends on other
! types of the program through its parent and its components (#14880).
program nested_vars_23
  implicit none
  type :: v_t
    integer :: k = 7
  end type
  type :: w_t
    real :: a = 1.5
    type(v_t) :: v
    type(v_t) :: varr(2)
  end type
  type :: base_t
    integer :: j = 3
    type(v_t), allocatable :: va
  end type
  type, extends(base_t) :: loc_t
    integer :: i = 4
    type(w_t) :: w
    type(w_t), allocatable :: wa(:)
    type(loc_t), pointer :: next => null()
    class(base_t), allocatable :: cb
  end type
  type(loc_t) :: s
  s%w = w_t(2.5, v_t(9), [v_t(1), v_t(2)])
  allocate(s%va)
  s%va%k = 11
  allocate(s%wa(3))
  s%wa(2)%v%k = 12
  allocate(loc_t :: s%cb)
  print *, f()
  if (s%i /= 4) error stop 1
  if (abs(s%w%a - 5.0) > 1e-6) error stop 2
  if (s%w%v%k /= 10) error stop 3
  if (s%w%varr(2)%k /= 2) error stop 4
  if (s%va%k /= 11) error stop 5
  if (s%wa(2)%v%k /= 13) error stop 6
  if (s%j /= 3) error stop 7
  if (associated(s%next)) error stop 8
  select type (c => s%cb)
  type is (loc_t)
    if (c%i /= 4) error stop 9
  class default
    error stop 10
  end select
  call scale()
  if (s%i /= 40) error stop 11
contains
  integer function f()
    f = s%i
    s%w%a = s%w%a * 2
    s%w%v%k = s%w%v%k + 1
    s%wa(2)%v%k = s%wa(2)%v%k + 1
  end function
  subroutine scale()
    type(w_t) :: x
    x = w_t(0.5, v_t(1), [v_t(3), v_t(4)])
    if (x%varr(1)%k /= 3) error stop 12
    s%i = s%i * 10
  end subroutine
end program
