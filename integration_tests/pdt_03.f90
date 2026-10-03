module pdt_03_module 
  implicit none
  !=================================================
  ! Origin PDT (used for nesting)
  !=================================================
  type :: origin(ik)
    integer, kind :: ik
    integer(kind=ik)  :: i_val(20)
  end type origin

  !=================================================
  ! Parent PDT
  !=================================================
  type :: parent(ik, rk)
    integer, kind :: ik
    integer, kind :: rk = kind(1.0)   ! default real kind
    integer(kind=ik) :: i_val
    real(kind=rk)    :: r_val
    type(origin(ik)) :: orig
    type(origin(rk)) :: orig2
  end type parent

  !=================================================
  ! Container PDT (nested)
  !=================================================
  type :: container(ik)
    integer, kind :: ik
    type(parent(ik, 8)) :: p      ! force 8-byte real
    type(parent(ik))    :: p2     ! default real kind
    type(parent(8, ik)) :: p3
    type(parent(8))     :: p4
  end type container
end module

program pdt_03
  use pdt_03_module
  use iso_fortran_env, only: int32, int64, real32, real64
  implicit none

  type(container(int32)) :: obj
  type(container(int64)) :: obj64

  integer(int64) :: big_val
  real(real64)   :: precise_val

  !=================================================
  ! 1. Parameter propagation tests
  !=================================================

  if (obj%ik /= int32) error stop "container ik incorrect"

  if (obj%p%ik /= int32) error stop "p%ik incorrect"
  if (obj%p%rk /= real64) error stop "p%rk should be real64"

  if (obj%p%orig%ik /= int32) error stop "p%orig%ik incorrect"
  if (obj%p%orig2%ik /= real64) error stop "p%orig2%ik should be real64"

  if (obj%p2%ik /= int32) error stop "p2%ik incorrect"
  if (obj%p2%rk /= kind(1.0)) error stop "p2%rk should be default real"

  if (obj%p2%orig%ik /= int32) error stop "p2%orig%ik incorrect"
  if (obj%p2%orig2%ik /= real32) error stop "p2%orig2%ik should be real32"

  if (obj%p3%ik /= int64) error stop "p3%ik incorrect"
  if (obj%p3%rk /= int32) error stop "p3%rk should be int32 (misuse of rk as ik in p3)"

  if (obj%p3%orig%ik /= int64) error stop "p3%orig%ik incorrect"
  if (obj%p3%orig2%ik /= real32) error stop "p3%orig2%ik should be real32"

  if (obj%p4%ik /= int64) error stop "p4%ik should be int64 (misuse of ik as rk in p4)"
  if (obj%p4%rk /= kind(1.0)) error stop "p4%rk should be default real"

  !=================================================
  ! 2. Integer storage tests (32-bit)
  !=================================================

  obj%p%i_val  = 123456
  obj%p2%i_val = -654321
  obj%p3%i_val = 12345678899_8

  if (obj%p%i_val /= 123456) error stop "p%i_val assignment failed"
  if (obj%p2%i_val /= -654321) error stop "p2%i_val assignment failed"
  if (obj%p3%i_val /= 12345678899_8) error stop "p3%i_val assignment failed"


  !=================================================
  ! 3. Real precision tests
  !=================================================

  precise_val = 1.0_real64 / 3.0_real64
  obj%p%r_val = precise_val
  obj%p2%r_val = 1.0 / 3.0
  obj%p3%r_val = 1.0 / 3.0

  if (abs(obj%p%r_val - precise_val) > 1.0e-15_real64) then
     error stop "High precision real lost in p"
  end if

  if (abs(obj%p2%r_val - (1.0/3.0)) > 1.0e-6) then
     error stop "Default precision real incorrect in p2"
  end if

  if (abs(obj%p3%r_val - (1.0/3.0)) > 1.0e-6) then
     error stop "Default precision real incorrect in p3"
  end if

  !=================================================
  ! 4. Storage size verification
  !=================================================

  if (storage_size(obj%p%i_val) /= 32) then
     error stop "Expected 32-bit integer storage"
  end if

  if (storage_size(obj%p%r_val) /= 64) then
     error stop "Expected 64-bit real storage in p"
  end if

  if (storage_size(obj%p3%i_val) /= 64) then
     error stop "Expected 64-bit integer storage in p3"
  end if

  !=================================================
  ! 5. Edge case: large 64-bit integer (should fail if wrong kind)
  !=================================================

  big_val = 2_int64**40   ! Requires 64-bit integer

  if (big_val <= 0) error stop "int64 not working correctly"

  ! Ensure 32-bit container cannot hold this value correctly
  obj%p%i_val = int(big_val, int32)

  if (int(obj%p%i_val, int64) == big_val) then
     error stop "Unexpected preservation of 64-bit value in 32-bit storage"
  end if

  if (obj64%ik /= int64) error stop "container(int64) failed"

  if (storage_size(obj64%p%i_val) /= 64) then
    error stop "Expected 64-bit integer storage in obj64"
  end if

  if (storage_size(obj64%p3%r_val) /= 64) then
    error stop "Expected 64-bit real storage in obj64%p3%r_val"
  end if

  print *, "All extended nested PDT tests passed successfully."

end program pdt_03