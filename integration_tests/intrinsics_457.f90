program intrinsics_457
  ! Test storage_size for derived types containing character components.
  ! storage_size must account for the character length, not just its kind.
  implicit none

  type :: t_char
    integer :: i
    logical :: f
    character(len=5) :: s
    complex :: c
  end type

  type :: t_char_only
    character(len=8) :: s
  end type

  type :: t_char_int
    character(len=3) :: s
    integer :: i
  end type

  type(t_char) :: x1
  type(t_char_only) :: x2
  type(t_char_int) :: x3

  ! 4 + 4 + 5 + 3(pad) + 8 = 24 bytes = 192 bits
  if (storage_size(x1) /= 192) error stop "t_char"

  ! 8 bytes = 64 bits
  if (storage_size(x2) /= 64) error stop "t_char_only"

  ! 3 + 1(pad) + 4 = 8 bytes = 64 bits
  if (storage_size(x3) /= 64) error stop "t_char_int"

  print *, "PASS"
end program
