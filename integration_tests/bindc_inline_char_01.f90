! Verify that bind(C) derived types with character(len>1) members are
! laid out inline (matching the C ABI), so c_loc + memcpy works
! correctly and storage_size reports the full inline byte count.
program bindc_inline_char_01
  use iso_c_binding, only: c_ptr, c_loc, c_size_t
  implicit none

  type :: object_t
    sequence
    integer :: i
    character(len=5) :: actor
    integer :: j
  end type

  interface
    subroutine memcpy(dest, src, n) bind(c, name="memcpy")
      import :: c_ptr, c_size_t
      type(c_ptr), value :: dest
      type(c_ptr), value :: src
      integer(c_size_t), value :: n
    end subroutine memcpy
  end interface

  type(object_t), target :: a, b
  integer(c_size_t) :: nbytes

  a%i = 7
  a%actor = "fooey"
  a%j = 99

  if (a%i /= 7) error stop "i"
  if (a%actor /= "fooey") error stop "actor"
  if (a%j /= 99) error stop "j"

  ! Padding shorter source string with blanks
  a%actor = "ab"
  if (a%actor /= "ab   ") error stop "padding"

  ! Truncating longer source string
  a%actor = "abcdefgh"
  if (a%actor /= "abcde") error stop "truncate"

  ! Round-trip via c_loc + memcpy: requires inline layout
  a%actor = "fooey"
  nbytes = int(storage_size(a) / 8, kind=c_size_t)
  call memcpy(c_loc(b), c_loc(a), nbytes)
  if (b%i /= 7) error stop "memcpy i"
  if (b%actor /= "fooey") error stop "memcpy actor"
  if (b%j /= 99) error stop "memcpy j"

  print *, "OK"
end program
