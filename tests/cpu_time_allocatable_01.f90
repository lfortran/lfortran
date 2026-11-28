program cpu_time_allocatable_01
  implicit none
  real, allocatable :: t4
  real(8), allocatable :: t8
  allocate(t4, t8)
  call cpu_time(t4)
  call cpu_time(t8)
end program
