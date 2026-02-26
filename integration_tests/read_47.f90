program read_47
  implicit none

  character(len=100) :: s
  integer :: x
  integer, allocatable :: a(:), b(:)

  ! Logical
  logical :: l
  logical, allocatable :: larr(:)

  ! int64
  integer(8) :: i64
  integer(8), allocatable :: i64arr(:)

  ! real(4)
  real :: r32
  real, allocatable :: r32arr(:)

  ! real(8)
  real(8) :: r64
  real(8), allocatable :: r64arr(:)

  ! complex(4)
  complex :: c32
  complex, allocatable :: c32arr(:)

  ! complex(8)
  complex(8) :: c64
  complex(8), allocatable :: c64arr(:)

  ! character
  character(5) :: ch
  character(5), allocatable :: charr(:)

  ! ----------------------------------------
  ! Test 1: Simple allocatable array
  ! ----------------------------------------
  allocate(a(3))
  s = "10 20 30"
  read(s, *) a
  if (any(a /= [10, 20, 30])) error stop "Test 1 failed"

  ! ----------------------------------------
  ! Test 2: Scalar + descriptor array
  ! ----------------------------------------
  deallocate(a)
  allocate(a(2))
  s = "5 10 20"
  read(s, *) x, a
  if (x /= 5) error stop "Test 2 failed (scalar)"
  if (any(a /= [10, 20])) error stop "Test 2 failed (array)"

  ! ----------------------------------------
  ! Test 3: Descriptor array + scalar
  ! ----------------------------------------
  deallocate(a)
  allocate(a(2))
  s = "10 20 30"
  read(s, *) a, x
  if (any(a /= [10, 20])) error stop "Test 3 failed (array)"
  if (x /= 30) error stop "Test 3 failed (scalar)"

  ! ----------------------------------------
  ! Test 4: Two descriptor arrays
  ! ----------------------------------------
  deallocate(a)
  allocate(a(2), b(2))
  s = "1 2 3 4"
  read(s, *) a, b
  if (any(a /= [1, 2])) error stop "Test 4 failed (a)"
  if (any(b /= [3, 4])) error stop "Test 4 failed (b)"

  ! ----------------------------------------
  ! Test 5: Comma-separated input
  ! ----------------------------------------
  deallocate(a)
  allocate(a(3))
  s = "7,8,9"
  read(s, *) a
  if (any(a /= [7, 8, 9])) error stop "Test 5 failed"

  ! ----------------------------------------
  ! Test 6: Mixed spaces and commas
  ! ----------------------------------------
  deallocate(a)
  allocate(a(3))
  s = "  11 ,  22   33 "
  read(s, *) a
  if (any(a /= [11, 22, 33])) error stop "Test 6 failed"

  ! ----------------------------------------
  ! Test 7: Logical
  ! ----------------------------------------

  ! Scalar logical
  s = "T"
  read(s, *) l
  if (.not. l) error stop "Test 7 failed (logical scalar T)"

  s = ".FALSE."
  read(s, *) l
  if (l) error stop "Test 7 failed (logical scalar F)"
   deallocate(larr)

  ! ----------------------------------------
  ! Test 8: int64
  ! ----------------------------------------
  s = "42"
  read(s, *) i64
  if (i64 /= 42_8) error stop "Test 8 failed (int64 scalar)"

  allocate(i64arr(2))
  s = "100 200"
  read(s, *) i64arr
  if (any(i64arr /= [100_8, 200_8])) &
       error stop "Test 8 failed (int64 array)"
  deallocate(i64arr)

  ! ----------------------------------------
  ! Test 9: real(4)
  ! ----------------------------------------
  s = "3.5"
  read(s, *) r32
  if (abs(r32 - 3.5) > 1e-6) error stop "Test 9 failed"

  allocate(r32arr(2))
  s = "1.5 2.5"
  read(s, *) r32arr
  if (any(abs(r32arr - [1.5, 2.5]) > 1e-6)) &
       error stop "Test 9 failed (array)"
  deallocate(r32arr)

  ! ----------------------------------------
  ! Test 10: real(8)
  ! ----------------------------------------
  s = "6.25"
  read(s, *) r64
  if (abs(r64 - 6.25_8) > 1d-12) error stop "Test 10 failed"

  allocate(r64arr(2))
  s = "10.0 20.0"
  read(s, *) r64arr
  if (any(abs(r64arr - [10.0_8, 20.0_8]) > 1d-12)) &
       error stop "Test 10 failed (array)"
  deallocate(r64arr)

  ! ----------------------------------------
  ! Test 11: complex(4)
  ! ----------------------------------------
  s = "(1.0,2.0)"
  read(s, *) c32
  if (abs(real(c32) - 1.0) > 1e-6 .or. &
      abs(aimag(c32) - 2.0) > 1e-6) &
      error stop "Test 11 failed"

  allocate(c32arr(2))
  s = "(3.0,4.0) (5.0,6.0)"
  read(s, *) c32arr
  if (abs(real(c32arr(1)) - 3.0) > 1e-6 .or. &
      abs(aimag(c32arr(1)) - 4.0) > 1e-6 .or. &
      abs(real(c32arr(2)) - 5.0) > 1e-6 .or. &
      abs(aimag(c32arr(2)) - 6.0) > 1e-6) &
      error stop "Test 11 failed (array)"
  deallocate(c32arr)

  ! ----------------------------------------
  ! Test 12: complex(8)
  ! ----------------------------------------
  s = "(7.0,8.0)"
  read(s, *) c64
  if (abs(real(c64) - 7.0_8) > 1d-12 .or. &
      abs(aimag(c64) - 8.0_8) > 1d-12) &
      error stop "Test 12 failed"

  allocate(c64arr(2))
  s = "(9.0,10.0) (11.0,12.0)"
  read(s, *) c64arr
  if (abs(real(c64arr(1)) - 9.0_8) > 1d-12 .or. &
      abs(aimag(c64arr(1)) - 10.0_8) > 1d-12 .or. &
      abs(real(c64arr(2)) - 11.0_8) > 1d-12 .or. &
      abs(aimag(c64arr(2)) - 12.0_8) > 1d-12) &
      error stop "Test 12 failed (array)"
  deallocate(c64arr)

  ! ----------------------------------------
  ! Test 13: character
  ! ----------------------------------------
  s = "abc"
  read(s, *) ch
  if (ch /= "abc  ") error stop "Test 13 failed (scalar)"

end program read_47