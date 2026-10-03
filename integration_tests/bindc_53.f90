! Extension: BIND(C) derived type with character(len>1) component.
! Not standard-conforming (F2023 C1806 requires length 1); GFortran rejects
! this, but Flang/ifx accept it. LFortran accepts it with a portability warning
! and lays the character out as inline [len x i8].
program bindc_53
  use iso_c_binding, only: c_ptr, c_loc, c_size_t, c_int, c_char
  implicit none

  type, bind(c) :: object_t
    integer(c_int) :: i
    character(kind=c_char, len=5) :: actor
    integer(c_int) :: j
  end type

  interface
    subroutine memcpy(dest, src, n) bind(c, name="memcpy")
      import :: c_ptr, c_size_t
      type(c_ptr), value :: dest
      type(c_ptr), value :: src
      integer(c_size_t), value :: n
    end subroutine memcpy
  end interface

  type(object_t), target :: obj1, obj2
  integer(c_size_t) :: bytes_to_copy
  integer :: ss

  obj1%i = 42
  obj1%actor = "fooey"
  obj1%j = 7

  ! Assignment / blank-pad / truncate for inline character member
  obj2%actor = "xx"
  if (obj2%actor /= "xx   ") error stop
  obj2%actor = "toolong"
  if (obj2%actor /= "toolo") error stop

  ss = storage_size(obj1)
  ! integer(4) + character(5) + integer(4) with alignment >= 13 bytes
  if (ss < 13 * 8) error stop

  bytes_to_copy = int(ss / 8, kind=c_size_t)
  call memcpy(c_loc(obj2), c_loc(obj1), bytes_to_copy)

  if (obj2%i /= 42) error stop
  if (obj2%actor /= "fooey") error stop
  if (obj2%j /= 7) error stop

  print *, "ok", ss
end program bindc_53
