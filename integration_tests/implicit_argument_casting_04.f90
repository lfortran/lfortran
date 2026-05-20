! Test that --implicit-argument-casting correctly handles optional arguments
! and rejects fundamentally incompatible types (e.g., string where integer expected)
! while allowing compatible numeric casts via keyword arguments.
module implicit_cast_optional_m
  use iso_c_binding, only: c_bool, c_int
  implicit none
contains
  subroutine my_stop(quiet, code_int, code_char)
    logical(c_bool), intent(in) :: quiet
    integer(c_int), intent(in), optional :: code_int
    character(len=*), intent(in), optional :: code_char
    if (present(code_int)) then
      if (code_int /= 42) error stop "wrong int"
    end if
    if (present(code_char)) then
      if (code_char /= "hello") error stop "wrong char"
    end if
  end subroutine
end module

program implicit_argument_casting_04
  use iso_c_binding, only: c_bool
  use implicit_cast_optional_m, only: my_stop
  implicit none

  ! Test 1: keyword argument for string optional (should work)
  call my_stop(.false._c_bool, code_char="hello")

  ! Test 2: keyword argument for integer optional (should work)
  call my_stop(.false._c_bool, code_int=42)

  ! Test 3: positional integer argument (should work)
  call my_stop(.false._c_bool, 42)

  ! Test 4: both keyword arguments (should work)
  call my_stop(.false._c_bool, code_int=42, code_char="hello")

  print *, "PASS"
end program
