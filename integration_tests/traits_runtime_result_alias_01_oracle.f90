! Standard Fortran counterpart of traits_runtime_result_alias_01: the
! interface is an abstract type with deferred bindings.
module traits_runtime_result_alias_01_oracle_m
    implicit none
    type, abstract :: IRhs
    contains
        procedure(rate_i), deferred :: rate
        procedure(total_i), deferred :: total
        procedure(gsum_i), deferred :: gsum
    end type
    abstract interface
        real(8) function rate_i(self, n, y)
            import IRhs
            class(IRhs), intent(in) :: self
            integer, intent(in) :: n
            real(8), intent(in) :: y(n)
        end function
        real(8) function total_i(self, x)
            import IRhs
            class(IRhs), intent(in) :: self
            real(8), intent(in) :: x(:)
        end function
        real(8) function gsum_i(self)
            import IRhs
            class(IRhs), intent(in) :: self
        end function
    end interface
    type, extends(IRhs) :: Decay
        real(8) :: k = 1
    contains
        procedure :: rate => decay_rate
        procedure :: total => decay_total
        procedure :: gsum => decay_gsum
    end type
    type :: State
        real(8) :: y(4)
        class(IRhs), allocatable :: f
    end type
    integer :: ncalls = 0
    real(8), allocatable :: gy(:)
contains
    real(8) function decay_rate(self, n, y)
        class(Decay), intent(in) :: self
        integer, intent(in) :: n
        real(8), intent(in) :: y(n)
        ncalls = ncalls + 1
        decay_rate = self%k * sum(y)
    end function
    real(8) function decay_total(self, x)
        class(Decay), intent(in) :: self
        real(8), intent(in) :: x(:)
        ncalls = ncalls + 1
        decay_total = self%k * sum(x)
    end function
    real(8) function decay_gsum(self)
        class(Decay), intent(in) :: self
        ncalls = ncalls + 1
        decay_gsum = self%k * sum(gy)
    end function

    subroutine check(y, e1, e2, e3, e4, calls)
        real(8), intent(in) :: y(:), e1, e2, e3, e4
        integer, intent(in) :: calls
        print '(4f8.2,i3)', y, ncalls
        if (any(abs(y - [e1, e2, e3, e4]) > 1d-12)) error stop 1
        if (ncalls /= calls) error stop 2
        ncalls = 0
    end subroutine

    ! A view; y is an assumed-shape dummy.
    subroutine step_assumed(f, y)
        class(IRhs), intent(in) :: f
        real(8), intent(inout) :: y(:)
        y = y + 0.5d0*f%total(y)
    end subroutine

    ! A view; y is an explicit-shape dummy.
    subroutine step_explicit(f, n, y)
        class(IRhs), intent(in) :: f
        integer, intent(in) :: n
        real(8), intent(inout) :: y(n)
        y = y + 0.5d0*f%rate(n, y)
    end subroutine

    ! An allocatable dummy.
    subroutine step_allocatable(f, y)
        class(IRhs), intent(in) :: f
        real(8), allocatable, intent(inout) :: y(:)
        y = y + 0.5d0*f%rate(size(y), y)
    end subroutine

    ! A module variable, also referenced only inside the implementation.
    subroutine step_module(f)
        class(IRhs), intent(in) :: f
        gy = gy + f%total(gy)
        call check(gy, 11d0, 12d0, 13d0, 14d0, 1)
        gy = [1d0, 2d0, 3d0, 4d0]
        gy = gy + f%gsum()
        call check(gy, 11d0, 12d0, 13d0, 14d0, 1)
    end subroutine

    ! An associate name of `class is`.
    subroutine step_class_is(object, y)
        class(*), intent(in) :: object
        real(8), intent(inout) :: y(:)
        select type (q => object)
        class is (IRhs)
            y = q%total(y) - y
        class default
            error stop 3
        end select
    end subroutine
end module

program traits_runtime_result_alias_01_oracle
    use traits_runtime_result_alias_01_oracle_m
    implicit none
    class(IRhs), allocatable :: f
    class(IRhs), pointer :: fp
    type(Decay), target :: d
    type(State) :: st
    real(8), allocatable :: y(:)
    real(8) :: z(4)
    real(8), target :: t(4)
    real(8), pointer :: p(:)

    allocate(f, source=Decay())
    fp => d
    allocate(y(4), gy(4))

    ! An owner; a local allocatable target.
    y = [1d0, 2d0, 3d0, 4d0]
    y = y + 0.5d0*f%rate(4, y)
    call check(y, 6d0, 7d0, 8d0, 9d0, 1)
    ! A local target that is not allocatable.
    z = [1d0, 2d0, 3d0, 4d0]
    z = f%total(z)
    call check(z, 10d0, 10d0, 10d0, 10d0, 1)
    z = [1d0, 2d0, 3d0, 4d0]
    z(2:4) = z(1:3) + f%total(z(2:4))
    call check(z, 1d0, 10d0, 11d0, 12d0, 1)

    z = [1d0, 2d0, 3d0, 4d0]
    call step_assumed(f, z)
    call check(z, 6d0, 7d0, 8d0, 9d0, 1)
    z = [1d0, 2d0, 3d0, 4d0]
    call step_explicit(f, 4, z)
    call check(z, 6d0, 7d0, 8d0, 9d0, 1)
    y = [1d0, 2d0, 3d0, 4d0]
    call step_allocatable(f, y)
    call check(y, 6d0, 7d0, 8d0, 9d0, 1)

    gy = [1d0, 2d0, 3d0, 4d0]
    call step_module(f)

    ! A component target and a component receiver.
    allocate(st%f, source=Decay(k=2))
    st%y = [1d0, 2d0, 3d0, 4d0]
    st%y = 2*st%f%total(st%y)
    call check(st%y, 40d0, 40d0, 40d0, 40d0, 1)

    ! A host variable assigned in an internal procedure.
    y = [1d0, 2d0, 3d0, 4d0]
    call inner()
    call check(y, 11d0, 12d0, 13d0, 14d0, 1)

    ! A pointer receiver; a pointer target associated with the argument.
    t = [1d0, 2d0, 3d0, 4d0]
    p => t
    p = p + fp%total(t)
    call check(t, 11d0, 12d0, 13d0, 14d0, 1)

    z = [1d0, 2d0, 3d0, 4d0]
    call step_class_is(d, z)
    call check(z, 9d0, 8d0, 7d0, 6d0, 1)

    ! Inside WHERE.
    z = [1d0, -2d0, 3d0, 4d0]
    where (z > 0)
        z = z + f%total(z)
    end where
    print '(4f8.2)', z
    if (any(z /= [7d0, -2d0, 9d0, 10d0])) error stop 4
contains
    subroutine inner()
        y = y + f%total(y)
    end subroutine
end program
