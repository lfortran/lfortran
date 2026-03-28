program intrinsics_450
  ! Test storage_size with proper struct alignment/padding.
  ! For bind(C) types, verify: storage_size(x) == c_sizeof(x) * 8
  ! For all struct types, verify: storage_size(x) == sizeof(x) * 8
  use iso_c_binding, only: c_ptr, c_null_ptr, c_int, c_double, c_sizeof
  implicit none

  ! --- Primitive types ---
  integer(1) :: i1
  integer(2) :: i2
  integer(4) :: i4
  integer(8) :: i8
  real(4)    :: r4
  real(8)    :: r8
  complex(4) :: c4
  complex(8) :: c8

  ! bind(C) struct: no padding needed  (4 + 4 = 8)
  type, bind(C) :: t_nopad
    integer(c_int) :: a
    integer(c_int) :: b
  end type

  ! bind(C) struct: padding between members
  ! 4 + 4(pad) + 8 + 4 + 4(pad) = 24
  type, bind(C) :: t_pad
    integer(c_int) :: a
    real(c_double) :: d
    integer(c_int) :: b
  end type

  ! bind(C) struct: no trailing padding  (8 + 4 + 4 = 16)
  type, bind(C) :: t_notrail
    real(c_double) :: d
    integer(c_int) :: a
    integer(c_int) :: b
  end type

  ! bind(C) struct with c_ptr  (4 + 4(pad) + 8 = 16)
  type, bind(C) :: t_cptr
    integer(c_int) :: a
    type(c_ptr)    :: p
  end type

  ! Single-member struct
  type, bind(C) :: t_single
    type(c_ptr) :: p
  end type

  ! Non-bind(C) struct with padding
  type :: t_fort
    integer(1) :: a
    real(8)    :: b
  end type

  ! Non-bind(C) struct: small fields only
  type :: t_small
    integer(1) :: a
    integer(1) :: b
    integer(1) :: c
  end type

  type(t_nopad)   :: x1
  type(t_pad)     :: x2
  type(t_notrail) :: x3
  type(t_cptr)    :: x4
  type(t_single)  :: x5
  type(t_fort)    :: x6
  type(t_small)   :: x7
  type(c_ptr)     :: cp

  ! Primitive checks
  if (storage_size(i1) /= 8)   error stop "i1"
  if (storage_size(i2) /= 16)  error stop "i2"
  if (storage_size(i4) /= 32)  error stop "i4"
  if (storage_size(i8) /= 64)  error stop "i8"
  if (storage_size(r4) /= 32)  error stop "r4"
  if (storage_size(r8) /= 64)  error stop "r8"
  if (storage_size(c4) /= 64)  error stop "c4"
  if (storage_size(c8) /= 128) error stop "c8"

  ! c_ptr
  cp = c_null_ptr
  if (storage_size(cp) /= 64) error stop "c_ptr"

  ! Struct: no padding
  if (storage_size(x1) /= 64) error stop "t_nopad"
  if (storage_size(x1) /= c_sizeof(x1) * 8) error stop "t_nopad vs c_sizeof"

  ! Struct: padding between int and double
  if (storage_size(x2) /= 192) error stop "t_pad"
  if (storage_size(x2) /= c_sizeof(x2) * 8) error stop "t_pad vs c_sizeof"

  ! Struct: no trailing padding
  if (storage_size(x3) /= 128) error stop "t_notrail"
  if (storage_size(x3) /= c_sizeof(x3) * 8) error stop "t_notrail vs c_sizeof"

  ! Struct with c_ptr member
  if (storage_size(x4) /= 128) error stop "t_cptr"
  if (storage_size(x4) /= c_sizeof(x4) * 8) error stop "t_cptr vs c_sizeof"

  ! Single-member struct
  if (storage_size(x5) /= 64) error stop "t_single"
  if (storage_size(x5) /= c_sizeof(x5) * 8) error stop "t_single vs c_sizeof"

  ! Non-bind(C) struct: sizeof() cross-check (uses LLVM DataLayout)
  if (storage_size(x6) /= sizeof(x6) * 8) error stop "t_fort vs sizeof"
  if (storage_size(x7) /= sizeof(x7) * 8) error stop "t_small vs sizeof"

  ! storage_size usable as parameter
  call check_param()

  ! --- Array checks ---
  ! For arrays: sizeof(x) == [storage_size(x(1)) / 8] * size(x)
  call check_arrays()

  print *, "PASS"
contains
  subroutine check_param()
    integer, parameter :: ss_i4 = storage_size(1_4)
    integer, parameter :: ss_i8 = storage_size(1_8)
    if (ss_i4 /= 32) error stop "param i4"
    if (ss_i8 /= 64) error stop "param i8"
  end subroutine

  subroutine check_arrays()
    integer(1) :: ai1(10)
    integer(4) :: ai4(5)
    integer(8) :: ai8(3)
    real(4)    :: ar4(7)
    real(8)    :: ar8(4)
    complex(4) :: ac4(2)
    complex(8) :: ac8(6)
    type(t_pad) :: at_pad(3)
    type(t_fort) :: at_fort(5)

    ! Primitive arrays
    if (sizeof(ai1) /= (storage_size(ai1(1)) / 8) * size(ai1)) &
        error stop "arr i1"
    if (sizeof(ai4) /= (storage_size(ai4(1)) / 8) * size(ai4)) &
        error stop "arr i4"
    if (sizeof(ai8) /= (storage_size(ai8(1)) / 8) * size(ai8)) &
        error stop "arr i8"
    if (sizeof(ar4) /= (storage_size(ar4(1)) / 8) * size(ar4)) &
        error stop "arr r4"
    if (sizeof(ar8) /= (storage_size(ar8(1)) / 8) * size(ar8)) &
        error stop "arr r8"
    if (sizeof(ac4) /= (storage_size(ac4(1)) / 8) * size(ac4)) &
        error stop "arr c4"
    if (sizeof(ac8) /= (storage_size(ac8(1)) / 8) * size(ac8)) &
        error stop "arr c8"

    ! Struct arrays (with padding)
    if (sizeof(at_pad) /= (storage_size(at_pad(1)) / 8) * size(at_pad)) &
        error stop "arr t_pad"
    if (sizeof(at_fort) /= (storage_size(at_fort(1)) / 8) * size(at_fort)) &
        error stop "arr t_fort"
  end subroutine
end program
