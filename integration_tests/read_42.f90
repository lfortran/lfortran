program read_42
  ! Internal (string-unit) reads into allocatable scalar and array variables.
  ! These used to crash with SIGSEGV because the codegen passed the alloca
  ! address (pointer-to-pointer) to the runtime instead of the allocated
  ! data pointer.
  implicit none
  character(len=3) :: s3
  character(len=7) :: s7
  real, allocatable :: x
  real, allocatable :: arr(:)

  ! Test 1: allocatable scalar
  s3 = "1.0"
  allocate(x)
  read(s3, fmt=*) x
  if (abs(x - 1.0) > 1e-6) error stop "Test 1 failed: allocatable scalar read"
  deallocate(x)

  ! Test 2: allocatable array
  s7 = "1, 2, 3"
  allocate(arr(3))
  read(s7, fmt=*) arr
  if (abs(arr(1) - 1.0) > 1e-6) error stop "Test 2 failed: arr(1)"
  if (abs(arr(2) - 2.0) > 1e-6) error stop "Test 2 failed: arr(2)"
  if (abs(arr(3) - 3.0) > 1e-6) error stop "Test 2 failed: arr(3)"
  deallocate(arr)

  print *, "All tests passed."
end program read_42
