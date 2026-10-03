! C_LOC accepts an assumed-rank actual argument (F2023 C840) and yields the
! address of the first element of the array.
program assumed_rank_15
  use, intrinsic :: iso_c_binding
  implicit none

  integer, target :: r1(5) = [1, 2, 3, 4, 5]
  integer, target :: r2(2, 2) = reshape([10, 20, 30, 40], [2, 2])
  real(c_double), target :: r3(2, 2, 2)
  integer, allocatable, target :: alloc(:)
  integer, target :: sec(6) = [1, 2, 3, 4, 5, 6]
  integer, target :: m(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])

  r3 = 0.0_c_double
  allocate(alloc(4))
  alloc = [7, 8, 9, 10]

  ! rank is only known at run time, so C_LOC must go through the descriptor
  call set_first_int(r1)
  if (r1(1) /= 99) error stop "rank 1"

  call set_first_int(r2)
  if (r2(1, 1) /= 99) error stop "rank 2"

  call set_first_real(r3)
  if (r3(1, 1, 1) /= 5.0_c_double) error stop "rank 3"

  call set_first_int(alloc)
  if (alloc(1) /= 99) error stop "allocatable"

  ! a contiguous section must give the address of the section, not of sec(1)
  call check_first_int(sec(3:5), 3)
  ! for a rank-2 array the first element in array element order is m(1, 1)
  call check_first_int(m, 1)

  print *, "ok"

contains

  subroutine set_first_int(a)
    integer, intent(inout), target, contiguous :: a(..)
    type(c_ptr) :: cptr
    integer, pointer :: fptr

    cptr = c_loc(a)
    call c_f_pointer(cptr, fptr)
    fptr = 99
  end subroutine set_first_int

  subroutine set_first_real(a)
    real(c_double), intent(inout), target, contiguous :: a(..)
    type(c_ptr) :: cptr
    real(c_double), pointer :: fptr

    cptr = c_loc(a)
    call c_f_pointer(cptr, fptr)
    fptr = 5.0_c_double
  end subroutine set_first_real

  subroutine check_first_int(a, expected)
    integer, intent(in), target, contiguous :: a(..)
    integer, intent(in) :: expected
    type(c_ptr) :: cptr
    integer, pointer :: fptr

    cptr = c_loc(a)
    call c_f_pointer(cptr, fptr)
    if (fptr /= expected) error stop "c_loc did not point at the first element"
  end subroutine check_first_int

end program assumed_rank_15
