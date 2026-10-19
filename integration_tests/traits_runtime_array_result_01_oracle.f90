! Standard Fortran counterpart of traits_runtime_array_result_01: the trait is
! an abstract type whose deferred binding returns an allocatable array.
module traits_runtime_array_result_01_oracle_m
    implicit none
    type, abstract :: ISamples
    contains
        procedure(samples_interface), deferred :: samples
    end type ISamples
    abstract interface
        function samples_interface(self) result(x)
            import :: ISamples
            class(ISamples), intent(in) :: self
            real(8), allocatable :: x(:)
        end function samples_interface
    end interface

    ! The first n positive integers.
    type, extends(ISamples) :: Ramp
        integer :: n = 0
    contains
        procedure :: samples => ramp_samples
    end type Ramp

    ! The values it holds.
    type, extends(ISamples) :: Recorded
        real(8), allocatable :: values(:)
    contains
        procedure :: samples => recorded_samples
    end type Recorded

    type :: Model
        class(ISamples), allocatable :: source
    end type Model
contains
    function ramp_samples(self) result(x)
        class(Ramp), intent(in) :: self
        real(8), allocatable :: x(:)
        integer :: i
        allocate(x(self%n))
        do i = 1, self%n
            x(i) = i
        end do
    end function ramp_samples

    function recorded_samples(self) result(x)
        class(Recorded), intent(in) :: self
        real(8), allocatable :: x(:)
        allocate(x, source=self%values)
    end function recorded_samples

    real(8) function total(x)
        real(8), intent(in) :: x(:)
        total = sum(x)
    end function total

    subroutine check(view, expected)
        class(ISamples), intent(in) :: view
        real(8), intent(in) :: expected(:)
        real(8), allocatable :: y(:), w(:)
        real(8) :: fixed(size(expected))
        integer :: i
        y = view%samples()
        if (size(y) /= size(expected)) error stop 1
        if (any(y /= expected)) error stop 2
        if (lbound(y, 1) /= 1) error stop 3
        if (size(view%samples()) /= size(expected)) error stop 4
        if (any(view%samples() * 2 /= 2 * expected)) error stop 5
        if (abs(total(view%samples()) - sum(expected)) > 1d-12) error stop 6
        if (abs(sum(view%samples() + view%samples()) - 2 * sum(expected)) > 1d-12) error stop 7
        fixed = view%samples()
        if (any(fixed /= expected)) error stop 8
        allocate(w(size(expected) + 1))
        w = [view%samples(), -1d0]
        if (size(w) /= size(expected) + 1 .or. w(size(w)) /= -1d0) error stop 9
        ! The result replaces an allocated target of another shape.
        deallocate(y)
        allocate(y(-3:10))
        y = view%samples()
        if (lbound(y, 1) /= 1 .or. size(y) /= size(expected)) error stop 10
        do i = 1, 3
            y = view%samples() + y
        end do
        if (any(y /= 4 * expected)) error stop 11
        print *, view%samples()
    end subroutine check
end module traits_runtime_array_result_01_oracle_m

program traits_runtime_array_result_01_oracle
    use traits_runtime_array_result_01_oracle_m
    implicit none
    class(ISamples), allocatable :: owner
    class(ISamples), pointer :: p
    type(Ramp), target :: r
    type(Model) :: m
    class(*), allocatable :: thing
    real(8), allocatable :: z(:)

    call check(Ramp(4), [1d0, 2d0, 3d0, 4d0])
    call check(Ramp(0), [real(8) ::])
    call check(Recorded([5d0, -1d0]), [5d0, -1d0])

    owner = Ramp(3)
    z = owner%samples()
    if (any(z /= [1d0, 2d0, 3d0])) error stop 20
    if (size(owner%samples()) /= 3) error stop 21
    call check(owner, [1d0, 2d0, 3d0])
    deallocate(owner)
    allocate(owner, source=Recorded([7d0, 8d0, 9d0, 10d0, 11d0]))
    if (sum(owner%samples()) /= 45d0) error stop 22

    r%n = 6
    p => r
    z = p%samples()
    if (size(z) /= 6 .or. z(6) /= 6d0) error stop 23
    r%n = 2
    if (size(p%samples()) /= 2) error stop 24
    call check(p, [1d0, 2d0])

    allocate(m%source, source=Ramp(5))
    z = m%source%samples()
    if (size(z) /= 5 .or. sum(z) /= 15d0) error stop 25
    call check(m%source, [1d0, 2d0, 3d0, 4d0, 5d0])

    allocate(thing, source=Recorded([2d0, 4d0]))
    select type (x => thing)
    class is (ISamples)
        z = x%samples()
        if (any(z /= [2d0, 4d0])) error stop 26
        if (size(x%samples()) /= 2) error stop 27
        call check(x, [2d0, 4d0])
    class default
        error stop 28
    end select
    print '(a)', 'traits_runtime_array_result_01_oracle ok'
end program traits_runtime_array_result_01_oracle
