! An allocatable character array declared alongside an interface body for a
! BIND(C) procedure that takes an assumed-size character dummy argument.
! Regression test: the dummy is a StringArraySinglePointer array, i.e. one flat
! character buffer, but its element String was left as DescriptorString. The two
! physical types disagreed, so the backend had to pick a representation on its
! own and ended up sharing one LLVM array descriptor between this dummy and the
! allocatable array, whose elements really are string descriptors.
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
