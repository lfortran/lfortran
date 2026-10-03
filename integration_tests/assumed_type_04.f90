program assumed_type_04
  implicit none

  type :: info_t
    integer :: offset
    integer :: size
    integer :: tag
  end type

  interface
    subroutine check_cfi_base_addr(a, expected_offset, expected_size, ok) bind(C)
      type(*), intent(in) :: a(..)
      integer, intent(in) :: expected_offset, expected_size
      integer, intent(out) :: ok
    end subroutine
  end interface

  type(info_t), target  :: direct
  type(info_t), pointer :: p
  integer :: ok

  direct = info_t(42, 100, 7)
  call check_cfi_base_addr(direct, 42, 100, ok)
  if (ok /= 1) error stop "direct struct: base_addr data mismatch"

  allocate(p)
  p = info_t(42, 100, 7)
  call check_cfi_base_addr(p, 42, 100, ok)
  if (ok /= 1) error stop "pointer deref: base_addr data mismatch"
  deallocate(p)

  print *, "ok"
end program
