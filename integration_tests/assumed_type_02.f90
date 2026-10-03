program assumed_type_02
  ! Test that the CFI type field is correctly set when a concrete type
  ! array is passed through a type(*) Fortran wrapper to a bind(C) routine.
  implicit none

  interface
    subroutine check_cfi_type_int32(a, ok) bind(C)
      type(*), intent(in) :: a(..)
      integer, intent(out) :: ok
    end subroutine

    subroutine check_cfi_type_real64(a, ok) bind(C)
      type(*), intent(in) :: a(..)
      integer, intent(out) :: ok
    end subroutine
  end interface

  integer, allocatable :: xi(:)
  real(8), allocatable :: xr(:)
  integer :: ok

  allocate(xi(3))
  xi = 42
  call forward_int(xi)

  allocate(xr(2))
  xr = 3.14d0
  call forward_real(xr)

  print *, "ok"

contains

  subroutine forward_int(a)
    type(*), intent(inout) :: a(..)
    integer :: ok
    call check_cfi_type_int32(a, ok)
    if (ok /= 1) error stop "CFI type for integer(4) is wrong"
  end subroutine

  subroutine forward_real(a)
    type(*), intent(inout) :: a(..)
    integer :: ok
    call check_cfi_type_real64(a, ok)
    if (ok /= 1) error stop "CFI type for real(8) is wrong"
  end subroutine

end program
