module derived_types_144_mod
  implicit none
  
  integer, parameter :: MINT = 8
  integer, parameter :: MVAR = MINT * 6 + 2 + (MINT - 1) * 3
  integer, parameter :: LIW = MVAR + 29
  integer, parameter :: LRW = 29 + (6 + MVAR) * MVAR
  
  type :: work_arrays_type
    real :: rwork(LRW) = 2.0
    integer :: iwork(LIW) = 1
    character(len=20) :: name = "default_name"
  end type work_arrays_type
  
  type(work_arrays_type) :: work_arrays
  
end module derived_types_144_mod

program derived_types_144
  use derived_types_144_mod
  implicit none
  
  print *, "LIW: ", size(work_arrays%iwork)
  print *, "LRW: ", size(work_arrays%rwork)
  print *, "Name: ", trim(work_arrays%name)

  if (size(work_arrays%iwork) /= LIW) error stop
  if (size(work_arrays%rwork) /= LRW) error stop
  if (trim(work_arrays%name) /= "default_name") error stop
  if (any(work_arrays%iwork /= 1)) error stop
  if (any(work_arrays%rwork /= 2.0)) error stop
  
end program derived_types_144
