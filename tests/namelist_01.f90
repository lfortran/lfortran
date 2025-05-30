program namelist_01
  implicit none
  real :: temperature
  integer :: max_iterations
  character(len=20) :: model_name

  namelist /config/ temperature, max_iterations, model_name
  temperature = 25.0
  max_iterations = 100
  model_name = "default_model"

  open(unit=10, file="input.txt", status="old")
  read(unit=10, nml=config)
  close(10)
end program
