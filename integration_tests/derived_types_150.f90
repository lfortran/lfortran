! Verify that SEQUENCE derived types with character(len>1) members are
! laid out inline, so storage_size reports the full inline byte count
! and c_loc + memcpy correctly copies the object representation.
program derived_types_150
  use iso_c_binding, only: c_ptr, c_loc, c_size_t
  implicit none

  type :: object_t
    sequence
    integer :: i
    logical :: fallacy
    character(len=5) :: actor
    complex :: issues
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
  integer :: ss1, ss2

  obj1%i = 42
  obj1%fallacy = .true.
  obj1%actor = "fooey"
  obj1%issues = cmplx(3.0, 4.0)

  ! Assignment into a SEQUENCE character component (inline [len x i8])
  obj2%actor = "xx"
  if (obj2%actor /= "xx   ") error stop
  obj2%actor = "toolong"
  if (obj2%actor /= "toolo") error stop

  ss1 = storage_size(obj1)
  ss2 = storage_size(obj2)
  if (ss1 /= ss2) error stop
  ! integer(4)+logical(4)+character(5)+complex(8) with alignment = 24 bytes = 192 bits
  ! (matches gfortran/flang; the old descriptor layout under-reported this).
  if (ss1 /= 192) error stop

  bytes_to_copy = int(ss1 / 8, kind=c_size_t)
  call memcpy(c_loc(obj2), c_loc(obj1), bytes_to_copy)
  call consume(obj2)

  if (obj2%i /= 42) error stop
  if (.not. obj2%fallacy) error stop
  if (obj2%actor /= "fooey") error stop
  if (abs(real(obj2%issues) - 3.0) > 1.0e-6) error stop
  if (abs(aimag(obj2%issues) - 4.0) > 1.0e-6) error stop

  print *, "ok", ss1

contains

  subroutine consume(value)
    class(*), intent(inout) :: value(..)
  end subroutine consume

end program derived_types_150
