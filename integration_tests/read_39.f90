program read_39
  implicit none
  integer(4) :: n32, ios
  integer(8) :: n64
  real(4) :: r32
  real(8) :: r64
  complex(4) :: c32
  complex(8) :: c64
  character(3) :: bad_input = 'BAD'
  character(3) :: int_str = '666'
  character(20) :: long_str

  ! ===== integer(4) tests =====
  read(bad_input, *, iostat=ios) n32
  if (ios <= 0) error stop "Test 1 failed: iostat should be positive for bad i32 input"
  read(int_str, *, iostat=ios) n32
  if (ios /= 0) error stop "Test 2 failed: iostat should be 0 for valid i32 input"
  if (n32 /= 666) error stop "Test 2 failed: n32 should be 666"

  ! ===== integer(8) tests =====
  read(bad_input, *, iostat=ios) n64
  if (ios <= 0) error stop "Test 3 failed: iostat should be positive for bad i64 input"
  long_str = '9876543210'
  read(long_str, *, iostat=ios) n64
  if (ios /= 0) error stop "Test 4 failed: iostat should be 0 for valid i64 input"
  if (n64 /= 9876543210_8) error stop "Test 4 failed: n64 value mismatch"

  ! ===== real(4) tests =====
  read(bad_input, *, iostat=ios) r32
  if (ios <= 0) error stop "Test 5 failed: iostat should be positive for bad f32 input"
  long_str = '3.14'
  read(long_str, *, iostat=ios) r32
  if (ios /= 0) error stop "Test 6 failed: iostat should be 0 for valid f32 input"
  if (abs(r32 - 3.14) > 0.01) error stop "Test 6 failed: r32 value mismatch"

  ! ===== real(8) tests =====
  read(bad_input, *, iostat=ios) r64
  if (ios <= 0) error stop "Test 7 failed: iostat should be positive for bad f64 input"
  long_str = '3.14159265358979'
  read(long_str, *, iostat=ios) r64
  if (ios /= 0) error stop "Test 8 failed: iostat should be 0 for valid f64 input"
  if (abs(r64 - 3.14159265358979d0) > 1.0d-10) error stop "Test 8 failed: r64 value mismatch"

  ! ===== complex(4) tests =====
  read(bad_input, *, iostat=ios) c32
  if (ios <= 0) error stop "Test 9 failed: iostat should be positive for bad c32 input"
  long_str = '(1.5, 2.5)'
  read(long_str, *, iostat=ios) c32
  if (ios /= 0) error stop "Test 10 failed: iostat should be 0 for valid c32 input"
  if (abs(real(c32) - 1.5) > 0.01) error stop "Test 10 failed: c32 real part mismatch"
  if (abs(aimag(c32) - 2.5) > 0.01) error stop "Test 10 failed: c32 imag part mismatch"

  ! ===== complex(8) tests =====
  read(bad_input, *, iostat=ios) c64
  if (ios <= 0) error stop "Test 11 failed: iostat should be positive for bad c64 input"
  long_str = '(1.5d0, 2.5d0)'
  read(long_str, *, iostat=ios) c64
  if (ios /= 0) error stop "Test 12 failed: iostat should be 0 for valid c64 input"
  if (abs(real(c64) - 1.5d0) > 1.0d-10) error stop "Test 12 failed: c64 real part mismatch"
  if (abs(aimag(c64) - 2.5d0) > 1.0d-10) error stop "Test 12 failed: c64 imag part mismatch"

end program read_39
