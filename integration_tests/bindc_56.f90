! The BIND(C) character kind must be recognised from the resolved symbol, not
! from how it is spelled: `C_char` (mixed case) and a renamed import both name
! the same `c_char` from iso_c_binding, so all three spellings must produce the
! same interoperable character type. Regression test: only the exact lowercase
! spelling `c_char` was recognised, so the other two produced a different
! physical type and the module failed LLVM verification.
module bindc_56_mod
  use ISO_C_Binding, only: C_char
  use iso_c_binding, only: renamed_char => c_char
  implicit none
contains
  subroutine upper_case(n, total)
    integer, intent(in) :: n
    integer, intent(out) :: total
    character(kind=C_char, len=1), allocatable :: buf(:)
    integer :: k
    interface
      subroutine c_upper(pattern) bind(C, name="c_upper")
        import
        character(kind=C_char) :: pattern(*)
      end subroutine c_upper
    end interface
    allocate(buf(n))
    do k = 1, n
      buf(k) = achar(iachar('A') + k - 1)
    end do
    total = 0
    do k = 1, n
      total = total + iachar(buf(k))
    end do
    deallocate(buf)
  end subroutine upper_case

  subroutine renamed_kind(n, total)
    integer, intent(in) :: n
    integer, intent(out) :: total
    character(kind=renamed_char, len=1), allocatable :: buf(:)
    integer :: k
    interface
      subroutine c_lower(pattern) bind(C, name="c_lower")
        import
        character(kind=renamed_char) :: pattern(*)
      end subroutine c_lower
    end interface
    allocate(buf(n))
    do k = 1, n
      buf(k) = achar(iachar('a') + k - 1)
    end do
    total = 0
    do k = 1, n
      total = total + iachar(buf(k))
    end do
    deallocate(buf)
  end subroutine renamed_kind
end module bindc_56_mod

program bindc_56
  use bindc_56_mod
  implicit none
  integer :: total

  call upper_case(3, total)
  print *, total
  ! iachar('A') + iachar('B') + iachar('C') = 65 + 66 + 67
  if (total /= 198) error stop

  call renamed_kind(3, total)
  print *, total
  ! iachar('a') + iachar('b') + iachar('c') = 97 + 98 + 99
  if (total /= 294) error stop
end program bindc_56
