program assumed_type_03
  ! MRE for caffeine co_broadcast double-free with LFortran.
  !
  ! Derived types with fixed-length character components must store the
  ! character data inline (like gfortran does) so that raw byte
  ! operations through type(*) / CFI descriptors work correctly.
  ! If the character is stored as a heap pointer instead, memcpy-style
  ! operations (e.g. caf_co_broadcast) copy the pointer, not the data,
  ! leading to double-free and wrong results across images.
  implicit none

  type :: pair_t
    integer :: id
    character(len=5) :: label
  end type

  interface
    subroutine check_dt_char_inline(a, ok) bind(C)
      type(*), intent(in) :: a(..)
      integer, intent(out) :: ok
    end subroutine

    subroutine copy_via_cfi(src, dst) bind(C)
      type(*), intent(in)    :: src(..)
      type(*), intent(inout) :: dst(..)
    end subroutine
  end interface

  type(pair_t) :: src, dst
  integer :: ok

  src = pair_t(42, "hello")

  call check_dt_char_inline(src, ok)
  if (ok /= 1) error stop "character data not stored inline in derived type"

  dst = pair_t(0, "xxxxx")
  call copy_via_cfi(src, dst)
  if (dst%id /= 42) error stop "integer component wrong after raw copy"
  if (dst%label /= "hello") error stop "character component wrong after raw copy"

  print *, "ok"
end program
