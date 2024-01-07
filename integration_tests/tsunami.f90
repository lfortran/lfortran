program tsunami

  ! This version solves the linearized 1-d advection equation:
  !
  !     du/dt + c du/dx = 0

  implicit none

  integer :: i, n

  integer, parameter :: grid_size = 100 ! grid size in x
  integer, parameter :: num_time_steps = 100 ! number of time steps

  real, parameter :: dt = 1 ! time step [s]
  real, parameter :: dx = 1 ! grid spacing [m]
  real, parameter :: c = 1 ! background flow speed [m/s]

  real :: h(grid_size), dh(grid_size)

  integer, parameter :: icenter = 25
  real, parameter :: decay = 0.02

  character(*), parameter :: fmt = '(i0,*(1x,es15.8e2))'

  ! check input parameter values
  if (grid_size <= 0) stop 'grid_size must be > 0'
  if (dt <= 0) stop 'time step dt must be > 0'
  if (dx <= 0) stop 'grid spacing dx must be > 0'
  if (c <= 0) stop 'background flow speed c must be > 0'

  ! initialize water height to a Gaussian shape
  do concurrent(i = 1:grid_size)
    h(i) = exp(-decay * (i - icenter)**2)
  end do

  ! write initial state to screen
  print fmt, 0, h

  time_loop: do n = 1, num_time_steps

    ! apply the periodic boundary condition
    dh(1) = h(1) - h(grid_size)

    ! calculate the difference of u in space
    do concurrent (i = 2:grid_size)
      dh(i) = h(i) - h(i-1)
    end do

    ! compute u at next time step
    do concurrent (i = 1:grid_size)
      h(i) = h(i) - c * dh(i) / dx * dt
    end do

    ! write current state to screen
    print fmt, n, h

  end do time_loop

end program tsunami