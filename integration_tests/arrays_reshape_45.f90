! Tests reshape() of a polymorphic (class) array passed to a class array dummy.
!
! Class arrays use a one-wrapper layout ({vptr, data_ptr} addressing a
! contiguous element buffer) rather than one wrapper per element, so reshape
! must deep-copy them through struct_deepcopy rather than the element-wise
! path used for plain derived types.
!
! Labelled llvm-only: gfortran 11 segfaults on this pattern (flang-new 19
! accepts it and prints the expected values).
program arrays_reshape_45
  implicit none

  type :: t
    integer :: i
  end type t

  class(t), allocatable :: a(:)

  allocate(a(1), source=t(1))
  call check1(reshape(a, [1]))

  deallocate(a)
  allocate(a(4), source=t(7))
  call check_all(reshape(a, [4]), 4, 7)

  ! reshape to a rank-2 shape
  call check_rank2(reshape(a, [2, 2]))

contains

  subroutine check1(arg)
    class(t) :: arg(:)
    if (size(arg) /= 1) error stop "check1: wrong size"
    if (arg(1)%i /= 1) error stop "check1: wrong value"
    print *, arg%i
  end subroutine check1

  subroutine check_all(arg, n, expected)
    class(t) :: arg(:)
    integer, intent(in) :: n, expected
    integer :: k
    if (size(arg) /= n) error stop "check_all: wrong size"
    do k = 1, n
      if (arg(k)%i /= expected) error stop "check_all: wrong value"
    end do
    print *, arg%i
  end subroutine check_all

  subroutine check_rank2(arg)
    class(t) :: arg(:,:)
    if (size(arg, 1) /= 2) error stop "check_rank2: wrong extent 1"
    if (size(arg, 2) /= 2) error stop "check_rank2: wrong extent 2"
    if (arg(1,1)%i /= 7) error stop "check_rank2: wrong value"
    if (arg(2,2)%i /= 7) error stop "check_rank2: wrong value"
    print *, arg(1,1)%i, arg(2,2)%i
  end subroutine check_rank2

end program arrays_reshape_45
