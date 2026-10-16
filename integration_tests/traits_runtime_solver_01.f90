! The solver interfaces of a user's module. IRhsFunc's sfunc marks no
! receiver, so it is receiverless and bound with nopass; a read-only
! class(IRhsFunc) view still selects its implementation, and the intent(out)
! derivative yp(:) is written into the caller's array, here also a strided
! section of it. ISolver's integrate borrows such a view and takes the
! explicit-shape y(neqn) and scalars without INTENT: one forward Euler step,
! through the concrete solver, an owning class(ISolver) and a borrowed
! intent(inout) view, updates y, t and iflag in place. A noncontiguous y is
! associated with y(neqn) by sequence, so only its own elements change.
module traits_runtime_solver_01_m
    implicit none

    abstract interface :: IRhsFunc
        subroutine sfunc(x,y,yp)
            real(8), intent(in)  :: x
            real(8), intent(in)  :: y(:)
            real(8), intent(out) :: yp(:)
        end subroutine sfunc
    end interface

    abstract interface :: ISolver
        subroutine integrate(self,rhs,neqn,y,t,tout,relerr,abserr,iflag)
            type(itself), pass, intent(inout) :: self
            class(IRhsFunc), intent(in)       :: rhs
            integer                           :: neqn, iflag
            real(8)                           :: t, tout, relerr, abserr
            real(8)                           :: y(neqn)
        end subroutine integrate
    end interface

    type :: Decay
    end type Decay
    implements IRhsFunc :: Decay
        procedure, nopass :: sfunc => decay_sfunc
    end implements

    type :: Drift
    end type Drift
    implements IRhsFunc :: Drift
        procedure, nopass :: sfunc => drift_sfunc
    end implements

    type :: Euler
        integer :: calls = 0
        real(8) :: last_h = 0
    end type Euler
    implements ISolver :: Euler
        procedure, pass :: integrate => euler_integrate
    end implements
contains
    subroutine decay_sfunc(x, y, yp)
        real(8), intent(in)  :: x
        real(8), intent(in)  :: y(:)
        real(8), intent(out) :: yp(:)
        yp = -2 * y
    end subroutine decay_sfunc

    subroutine drift_sfunc(x, y, yp)
        real(8), intent(in)  :: x
        real(8), intent(in)  :: y(:)
        real(8), intent(out) :: yp(:)
        integer :: i
        do i = 1, size(yp)
            yp(i) = x + i * y(i)
        end do
    end subroutine drift_sfunc

    ! One forward Euler step from t to tout; negative tolerances are
    ! rejected with iflag = 6, leaving everything else unchanged.
    subroutine euler_integrate(self, rhs, neqn, y, t, tout, relerr, abserr, iflag)
        class(Euler), intent(inout) :: self
        class(IRhsFunc), intent(in) :: rhs
        integer :: neqn, iflag
        real(8) :: t, tout, relerr, abserr
        real(8) :: y(neqn)
        real(8) :: yp(neqn)
        if (relerr < 0 .or. abserr < 0) then
            iflag = 6
            return
        end if
        yp = huge(1d0)
        call rhs%sfunc(t, y, yp)
        y = y + (tout - t) * yp
        self%calls = self%calls + 1
        self%last_h = tout - t
        t = tout
        iflag = 2
    end subroutine euler_integrate

    ! The derivative through a read-only view, into every other element of
    ! the caller's array.
    subroutine probe(rhs, x, y, out)
        class(IRhsFunc), intent(in) :: rhs
        real(8), intent(in) :: x, y(:)
        real(8), intent(inout) :: out(:)
        call rhs%sfunc(x, y, out(2::2))
    end subroutine probe

    ! A borrowed mutable solver; y(:) may be noncontiguous.
    subroutine advance(solver, rhs, n, y, t, tout, iflag)
        class(ISolver), intent(inout) :: solver
        class(IRhsFunc), intent(in) :: rhs
        integer, intent(in) :: n
        real(8), intent(inout) :: y(:), t
        real(8), intent(in) :: tout
        integer, intent(out) :: iflag
        integer :: m
        real(8) :: stop_time, relerr, abserr
        m = n
        stop_time = tout
        relerr = 0
        abserr = 0
        iflag = 1
        call solver%integrate(rhs, m, y, t, stop_time, relerr, abserr, iflag)
    end subroutine advance
end module traits_runtime_solver_01_m

program traits_runtime_solver_01
    use traits_runtime_solver_01_m
    implicit none
    type(Euler) :: concrete
    type(Decay) :: decay_rhs
    class(ISolver), allocatable :: solver
    class(IRhsFunc), allocatable :: rhs
    real(8) :: y(3), y4(4), ys(6), buf(6), t, tout, relerr, abserr
    integer :: n, iflag

    ! The receiverless message writes the caller's array through a view.
    buf = -1
    call probe(Drift(), 0.5d0, [1d0, 2d0, 3d0], buf)
    if (any(buf /= [-1d0, 1.5d0, -1d0, 4.5d0, -1d0, 9.5d0])) error stop 1
    allocate(rhs, source=Decay())
    buf = -1
    call probe(rhs, 0d0, [1d0, 2d0, 3d0], buf)
    if (any(buf /= [-1d0, -2d0, -1d0, -4d0, -1d0, -6d0])) error stop 2
    buf = -1
    call rhs%sfunc(0d0, [1d0, 2d0, 3d0], buf(6:2:-2))
    if (any(buf /= [-1d0, -6d0, -1d0, -4d0, -1d0, -2d0])) error stop 3

    ! The concrete solver, statically.
    n = 3
    y = [1d0, 2d0, 3d0]
    t = 0
    tout = 0.25d0
    relerr = 1d-6
    abserr = 1d-6
    iflag = 1
    call concrete%integrate(decay_rhs, n, y, t, tout, relerr, abserr, iflag)
    if (any(y /= [0.5d0, 1d0, 1.5d0])) error stop 4
    if (t /= 0.25d0 .or. iflag /= 2) error stop 5
    if (concrete%calls /= 1 .or. concrete%last_h /= 0.25d0) error stop 6

    ! An owning view selects Euler's integrate, which calls Drift's sfunc.
    allocate(solver, source=Euler())
    deallocate(rhs)
    allocate(rhs, source=Drift())
    y = [1d0, 2d0, 3d0]
    t = 1
    tout = 1.5d0
    iflag = 1
    call solver%integrate(rhs, n, y, t, tout, relerr, abserr, iflag)
    if (any(y /= [2d0, 4.5d0, 8d0])) error stop 7
    if (t /= 1.5d0 .or. iflag /= 2) error stop 8

    ! A strided section and a shorter neqn: only y(1:neqn) of the sequence.
    ys = [1d0, 10d0, 2d0, 20d0, 3d0, 30d0]
    t = 0
    tout = 0.25d0
    call solver%integrate(decay_rhs, n, ys(1::2), t, tout, relerr, abserr, iflag)
    if (any(ys /= [0.5d0, 10d0, 1d0, 20d0, 1.5d0, 30d0])) error stop 9
    y4 = [4d0, 8d0, 12d0, 16d0]
    t = 0
    n = 2
    call solver%integrate(decay_rhs, n, y4, t, tout, relerr, abserr, iflag)
    if (any(y4 /= [2d0, 4d0, 12d0, 16d0]) .or. iflag /= 2) error stop 10

    ! Rejected input changes only iflag.
    relerr = -1
    t = 0
    call solver%integrate(rhs, n, y4, t, tout, relerr, abserr, iflag)
    if (iflag /= 6 .or. t /= 0 .or. any(y4 /= [2d0, 4d0, 12d0, 16d0])) error stop 11
    select type (s => solver)
    type is (Euler)
        if (s%calls /= 3 .or. s%last_h /= 0.25d0) error stop 12
    class default
        error stop 13
    end select

    ! A borrowed intent(inout) view of the concrete solver; its y(:) is a
    ! noncontiguous section, associated with y(neqn) by sequence.
    ys = [1d0, 10d0, 2d0, 20d0, 3d0, 30d0]
    t = 0
    call advance(concrete, Decay(), 3, ys(2::2), t, 0.25d0, iflag)
    if (any(ys /= [1d0, 5d0, 2d0, 10d0, 3d0, 15d0])) error stop 14
    if (t /= 0.25d0 .or. iflag /= 2 .or. concrete%calls /= 2) error stop 15
    call advance(solver, rhs, 3, ys(5:1:-2), t, 0.5d0, iflag)
    if (any(ys /= [1.8125d0, 5d0, 3.0625d0, 10d0, 3.8125d0, 15d0])) error stop 16
    if (t /= 0.5d0 .or. iflag /= 2) error stop 17
    select type (s => solver)
    type is (Euler)
        if (s%calls /= 4) error stop 18
    class default
        error stop 19
    end select
    print '(a)', 'traits_runtime_solver_01 ok'
end program traits_runtime_solver_01
