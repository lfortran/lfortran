! Standard Fortran counterpart of traits_runtime_polymorphic_source_02: the
! contract IStep is an abstract type that Euler extends.
module traits_runtime_polymorphic_source_02_oracle_m
    implicit none
    integer :: euler_finals = 0, traced_finals = 0
    type, abstract :: IStep
    contains
        procedure(step_interface), deferred :: step
        procedure(count_interface), deferred :: count
    end type
    abstract interface
        subroutine step_interface(self, y)
            import :: IStep
            class(IStep), intent(inout) :: self
            real(8), intent(inout) :: y
        end subroutine
        integer function count_interface(self)
            import :: IStep
            class(IStep), intent(in) :: self
        end function
    end interface

    type, extends(IStep) :: Euler
        real(8) :: h = 0.5d0
        integer :: nsteps = 0
    contains
        procedure :: step => euler_step
        procedure :: count => euler_count
        final :: euler_final
    end type

    type, extends(Euler) :: TracedEuler
        integer, allocatable :: log(:)
    contains
        procedure :: step => traced_step
        procedure :: count => traced_count
        final :: traced_final
    end type
contains
    subroutine euler_step(self, y)
        class(Euler), intent(inout) :: self
        real(8), intent(inout) :: y
        y = y + self%h
        self%nsteps = self%nsteps + 1
    end subroutine

    integer function euler_count(self)
        class(Euler), intent(in) :: self
        euler_count = self%nsteps
    end function

    subroutine traced_step(self, y)
        class(TracedEuler), intent(inout) :: self
        real(8), intent(inout) :: y
        integer, allocatable :: log(:)
        y = y + 10*self%h
        self%nsteps = self%nsteps + 1
        allocate(log(size(self%log) + 1))
        log(:size(self%log)) = self%log
        log(size(log)) = self%nsteps
        call move_alloc(log, self%log)
    end subroutine

    integer function traced_count(self)
        class(TracedEuler), intent(in) :: self
        traced_count = 1000*size(self%log) + self%nsteps
    end function

    subroutine euler_final(self)
        type(Euler), intent(inout) :: self
        euler_finals = euler_finals + 1
    end subroutine

    subroutine traced_final(self)
        type(TracedEuler), intent(inout) :: self
        traced_finals = traced_finals + 1
    end subroutine

    integer function kind_of(s)
        class(IStep), intent(in) :: s
        select type (concrete => s)
        type is (TracedEuler)
            kind_of = 2
        type is (Euler)
            kind_of = 1
        class default
            kind_of = 0
        end select
    end function

    ! Associates a trait pointer with a class(Euler) target dummy.
    subroutine point(e, p)
        class(Euler), target, intent(inout) :: e
        class(IStep), pointer, intent(out) :: p
        p => e
    end subroutine

    ! Copies a class(Euler) dummy into owners.
    subroutine own(e, o, a)
        class(Euler), intent(in) :: e
        class(IStep), allocatable, intent(out) :: o, a
        allocate(o, source=e)
        a = e
    end subroutine
end module

program traits_runtime_polymorphic_source_02_oracle
    use traits_runtime_polymorphic_source_02_oracle_m
    implicit none
    type(TracedEuler), target :: t
    class(Euler), pointer :: q
    class(IStep), pointer :: p, r
    class(IStep), allocatable :: o, a
    real(8) :: y

    allocate(t%log(0))
    call point(t, p)
    y = 0
    call p%step(y)
    if (abs(y - 5) > 1d-12 .or. t%nsteps /= 1 .or. size(t%log) /= 1) error stop 1
    if (p%count() /= 1001 .or. kind_of(p) /= 2) error stop 2
    select type (x => p)
    type is (TracedEuler)
        x%nsteps = 10
    class default
        error stop 3
    end select
    if (t%nsteps /= 10) error stop 4
    q => t
    r => q
    y = 0
    call r%step(y)
    if (abs(y - 5) > 1d-12 .or. t%nsteps /= 11 .or. size(t%log) /= 2) error stop 5
    if (.not. associated(r) .or. kind_of(r) /= 2) error stop 6
    nullify(p)
    if (r%count() /= 2011) error stop 7

    ! Owners copy the whole dynamic type and are independent of it.
    allocate(o, source=q)
    if (euler_finals /= 0 .or. traced_finals /= 0) error stop 8
    y = 0
    call o%step(y)
    if (abs(y - 5) > 1d-12 .or. t%nsteps /= 11 .or. size(t%log) /= 2) error stop 9
    if (o%count() /= 3012 .or. kind_of(o) /= 2) error stop 10
    select type (x => o)
    type is (TracedEuler)
        if (any(x%log /= [1, 11, 12])) error stop 11
    class default
        error stop 12
    end select
    a = q
    call a%step(y)
    call a%step(y)
    if (a%count() /= 4013 .or. t%nsteps /= 11 .or. o%count() /= 3012) error stop 13
    if (euler_finals /= 0 .or. traced_finals /= 0) error stop 14
    deallocate(o)
    if (euler_finals /= 1 .or. traced_finals /= 1) error stop 15
    deallocate(a)
    if (euler_finals /= 2 .or. traced_finals /= 2) error stop 16

    ! The same through a class(Euler) dummy.
    call own(q, o, a)
    if (euler_finals /= 2 .or. traced_finals /= 2) error stop 17
    call o%step(y)
    if (o%count() /= 3012 .or. a%count() /= 2011 .or. t%nsteps /= 11) error stop 18
    if (kind_of(o) /= 2 .or. kind_of(a) /= 2) error stop 19
    deallocate(o, a)
    if (euler_finals /= 4 .or. traced_finals /= 4) error stop 20
    print *, "PASS"
end program
