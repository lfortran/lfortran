! The consumer, compiled without the providers, drives their solver with
! their right-hand sides through views: the derivative reaches a strided
! section of the caller's array, and noncontiguous y reach the explicit-shape
! y(neqn) of integrate by sequence, through a borrowed concrete solver and an
! owning class(ISolver).
program traits_runtime_solver_separate_01
    use traits_runtime_solver_separate_01_contracts_m, only: IRhsFunc, ISolver
    use traits_runtime_solver_separate_01_consumer_m, only: drive, derivative
    use traits_runtime_solver_separate_01_provider_m, only: Decay, Ramp, Euler
    implicit none
    type(Euler) :: concrete
    class(ISolver), allocatable :: solver
    class(IRhsFunc), allocatable :: rhs
    real(8), allocatable :: y(:)
    real(8) :: buf(6), t
    integer :: iflag, steps

    buf = -1
    call derivative(Decay(), 0d0, [1d0, 2d0, 3d0], buf(1::2))
    if (any(buf /= [-2d0, -1d0, -4d0, -1d0, -6d0, -1d0])) error stop 1
    allocate(rhs, source=Ramp())
    call derivative(rhs, 0.5d0, [1d0, 2d0, 3d0], buf(6:2:-2))
    if (any(buf /= [-2d0, 1.5d0, -4d0, 1d0, -6d0, 0.5d0])) error stop 2

    ! Two steps of h = 0.25 halve y twice; only y(1::2) is the sequence.
    allocate(y(6))
    y = [1d0, 10d0, 2d0, 20d0, 3d0, 30d0]
    t = 0
    steps = drive(concrete, Decay(), y(1::2), t, [0.25d0, 0.5d0], iflag)
    if (steps /= 2 .or. iflag /= 2 .or. t /= 0.5d0) error stop 3
    if (any(y /= [0.25d0, 10d0, 0.5d0, 20d0, 0.75d0, 30d0])) error stop 4
    if (concrete%steps /= 2) error stop 5

    ! An owning view of the provider's Euler with Ramp, yp(i) = i*t, steps
    ! the reversed sequence once and then rejects a tout before t.
    allocate(solver, source=Euler())
    steps = drive(solver, rhs, y(6:2:-2), t, [1d0, 0.25d0], iflag)
    if (steps /= 1 .or. iflag /= 6 .or. t /= 1d0) error stop 6
    if (any(y /= [0.25d0, 10.75d0, 0.5d0, 20.5d0, 0.75d0, 30.25d0])) error stop 7
    select type (s => solver)
    type is (Euler)
        if (s%steps /= 1) error stop 8
    class default
        error stop 9
    end select
    print '(a)', 'traits_runtime_solver_separate_01 ok'
end program traits_runtime_solver_separate_01
