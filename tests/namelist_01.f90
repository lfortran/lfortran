program namelist_01
  implicit none
  real :: temperature
  integer :: max_iterations
  character(len=20) :: model_name

  namelist /config/ temperature, max_iterations, model_name
end program
