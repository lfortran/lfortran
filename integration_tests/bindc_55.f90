! An allocatable character array declared alongside an interface body for a
! BIND(C) procedure that takes an assumed-size character dummy argument.
! Regression test: both arrays encode to the ASR type code "str" with rank 1,
! but the allocatable one has %string_descriptor elements while the BIND(C) one
! has i8* elements. LFortran cached a single LLVM array descriptor struct for
! both, so it stored a %string_descriptor* into an i8** field and the generated
! module failed LLVM verification.
module bindc_55_mod
  implicit none
contains
  subroutine fill(n, total)
    integer, intent(in) :: n
    integer, intent(out) :: total
    character, allocatable :: buf(:)
    integer :: k
    interface
      subroutine c_take(pattern) bind(c, name="c_take")
        character :: pattern(*)
      end subroutine c_take
    end interface
    allocate(buf(n))
    do k = 1, n
      buf(k) = achar(iachar('a') + k - 1)
    end do
    if (buf(1) /= 'a') error stop
    if (buf(n) /= achar(iachar('a') + n - 1)) error stop
    total = size(buf)
    do k = 1, n
      total = total + iachar(buf(k))
    end do
    deallocate(buf)
  end subroutine fill
end module bindc_55_mod

program bindc_55
  use bindc_55_mod
  implicit none
  integer :: total

  call fill(3, total)
  print *, total
  ! size + iachar('a') + iachar('b') + iachar('c') = 3 + 97 + 98 + 99
  if (total /= 297) error stop

  call fill(1, total)
  print *, total
  ! size + iachar('a') = 1 + 97
  if (total /= 98) error stop
end program bindc_55
