program bindc_char_len_01
  use iso_c_binding, only: c_int, c_char
  implicit none

  type, bind(c) :: t
    integer(c_int) :: i
    character(kind=c_char, len=5) :: s
  end type

  type(t) :: x
  x%i = 1
  x%s = "hello"
  print *, x%i, x%s
end program
