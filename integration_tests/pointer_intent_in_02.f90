program pointer_intent_in_02
  use iso_c_binding, only: c_ptr, c_loc, c_associated
  implicit none
  ! A POINTER, INTENT(IN) array dummy that is given a non-pointer, non-
  ! allocatable array with the TARGET attribute becomes associated with it,
  ! keeping its shape and bounds.
  integer, parameter :: dp = kind(1.0d0)
  real(dp), target :: a(4, 5)
  integer, target :: c(0:3)
  character(len=3), target :: ch(2)
  real(dp), allocatable, target :: b(:,:)
  integer :: r
  abstract interface
    subroutine sum_iface(p, r)
      integer, pointer, intent(in) :: p(:)
      integer, intent(out) :: r
    end subroutine
  end interface
  procedure(sum_iface), pointer :: fp
  a = 1.0_dp
  a(4, 5) = 7.0_dp
  c = [10, 11, 12, 13]
  ch = ["abc", "xyz"]
  allocate(b(3, 7))
  b = 2.0_dp

  call check_2d(a, 4, 5, 1.0_dp, 7.0_dp)
  call check_2d(b, 3, 7, 2.0_dp, 2.0_dp)
  call check_loc(a)
  call check_int(c)
  call check_char(ch)
  call wrap_2d(a)
  call wrap_1d(c)
  fp => sum_ptr
  call fp(c, r)
  if (r /= 46) error stop "wrong sum through procedure pointer"

contains

  subroutine check_2d(d, n1, n2, first, last)
    real(dp), intent(in), pointer, contiguous :: d(:,:)
    integer, intent(in) :: n1, n2
    real(dp), intent(in) :: first, last
    if (.not. associated(d)) error stop "d is not associated"
    if (any(shape(d) /= [n1, n2])) error stop "wrong shape"
    if (size(d, 1) /= n1 .or. size(d, 2) /= n2) error stop "wrong size"
    if (any(lbound(d) /= 1)) error stop "wrong lbound"
    if (ubound(d, 1) /= n1 .or. ubound(d, 2) /= n2) error stop "wrong ubound"
    if (d(1, 1) /= first) error stop "wrong first element"
    if (d(n1, n2) /= last) error stop "wrong last element"
  end subroutine

  subroutine check_loc(d)
    real(dp), intent(in), pointer, contiguous :: d(:,:)
    type(c_ptr) :: cp
    cp = c_loc(d(1, 1))
    if (.not. c_associated(cp)) error stop "c_loc is null"
    if (.not. c_associated(cp, c_loc(a))) error stop "c_loc differs"
  end subroutine

  subroutine check_int(p)
    integer, intent(in), pointer :: p(:)
    if (.not. associated(p, c)) error stop "p is not associated with c"
    if (size(p) /= 4) error stop "wrong size of p"
    if (lbound(p, 1) /= 0 .or. ubound(p, 1) /= 3) error stop "wrong bounds of p"
    if (p(0) /= 10 .or. p(3) /= 13) error stop "wrong elements of p"
  end subroutine

  subroutine check_char(p)
    character(len=*), intent(in), pointer :: p(:)
    if (size(p) /= 2) error stop "wrong size of character p"
    if (len(p) /= 3) error stop "wrong len of character p"
    if (p(2) /= "xyz") error stop "wrong element of character p"
  end subroutine

  subroutine wrap_2d(x)
    real(dp), target, contiguous :: x(:,:)
    call check_2d(x, 4, 5, 1.0_dp, 7.0_dp)
  end subroutine

  subroutine wrap_1d(x)
    integer, target :: x(:)
    call check_shape_1d(x, 4)
  end subroutine

  subroutine check_shape_1d(p, n)
    integer, intent(in), pointer :: p(:)
    integer, intent(in) :: n
    if (size(p) /= n .or. lbound(p, 1) /= 1) error stop "wrong shape of 1d p"
    if (p(1) /= 10 .or. p(n) /= 13) error stop "wrong elements of 1d p"
  end subroutine

  subroutine sum_ptr(p, r)
    integer, pointer, intent(in) :: p(:)
    integer, intent(out) :: r
    if (lbound(p, 1) /= 0) error stop "wrong lbound through procedure pointer"
    r = sum(p)
  end subroutine

end program
