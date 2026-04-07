program gpu_metal_151
! Test: gpu_offload pass loads transitive module dependencies when
! a submodule uses a module that itself uses another module.
! Previously, load_submodule_deps only loaded direct dependencies
! of the submodule TU, leaving ExternalSymbol entries with null
! m_external in indirectly loaded modules.
  use gpu_metal_151_network_m, only : network_t
  implicit none
  type(network_t) :: net
  real :: x(4), y(4)
  integer :: i
  x = [1.0, 2.0, 3.0, 4.0]
  do concurrent(i=1:4)
    y(i) = net%infer(x(i))
  end do
  print *, y
  if (abs(y(1) - 1.0) > 1e-6) error stop
  if (abs(y(2) - 2.0) > 1e-6) error stop
  if (abs(y(3) - 3.0) > 1e-6) error stop
  if (abs(y(4) - 4.0) > 1e-6) error stop
end program
