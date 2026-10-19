! Standard Fortran counterpart of traits_runtime_polymorphic_source_separate_01
! in one file: the contract IStep is an abstract type that Euler extends.
module poly_source_separate_01_oracle_contracts_m
    implicit none
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
end module

module poly_source_separate_01_oracle_consumer_m
    use poly_source_separate_01_oracle_contracts_m, only: IStep
    implicit none
    private
    public :: advance, peek, advance_pointer
contains
    subroutine advance(s, y)
        class(IStep), intent(inout) :: s
        real(8), intent(inout) :: y
        call s%step(y)
    end subroutine

    integer function peek(s)
        class(IStep), intent(in) :: s
        peek = s%count()
    end function

    subroutine advance_pointer(p, y)
        class(IStep), pointer, intent(inout) :: p
        real(8), intent(inout) :: y
        call advance(p, y)
    end subroutine
end module

module poly_source_separate_01_oracle_provider_m
    use poly_source_separate_01_oracle_contracts_m, only: IStep
    use poly_source_separate_01_oracle_consumer_m, only: advance, peek
    implicit none
    private
    public :: Euler, drive, count_of

    type, extends(IStep) :: Euler
        real(8) :: h = 0.5d0
        integer :: nsteps = 0
    contains
        procedure :: step => euler_step
        procedure :: count => euler_count
        procedure :: run => euler_run
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

    subroutine euler_run(self, y)
        class(Euler), intent(inout) :: self
        real(8), intent(inout) :: y
        call advance(self, y)
    end subroutine

    subroutine drive(e, y)
        class(Euler), intent(inout) :: e
        real(8), intent(inout) :: y
        call advance(e, y)
    end subroutine

    integer function count_of(e)
        class(Euler), intent(in) :: e
        count_of = peek(e)
    end function
end module

module poly_source_separate_01_oracle_extension_m
    use poly_source_separate_01_oracle_provider_m, only: Euler
    implicit none
    private
    public :: TracedEuler

    type, extends(Euler) :: TracedEuler
        integer, allocatable :: log(:)
    contains
        procedure :: step => traced_step
        procedure :: count => traced_count
    end type
contains
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
end module

program traits_runtime_polymorphic_source_separate_01_oracle
    use poly_source_separate_01_oracle_contracts_m, only: IStep
    use poly_source_separate_01_oracle_consumer_m, only: advance, peek, &
        advance_pointer
    use poly_source_separate_01_oracle_provider_m, only: Euler, drive, &
        count_of
    use poly_source_separate_01_oracle_extension_m, only: TracedEuler
    implicit none
    class(Euler), allocatable, target :: c
    type(Euler) :: e
    class(IStep), pointer :: p
    class(IStep), allocatable :: o
    real(8) :: y

    allocate(TracedEuler :: c)
    select type (x => c)
    type is (TracedEuler)
        allocate(x%log(0))
    end select
    y = 0
    call c%run(y)
    if (abs(y - 5) > 1d-12 .or. c%nsteps /= 1) error stop 1
    y = 0
    call drive(c, y)
    if (abs(y - 5) > 1d-12 .or. count_of(c) /= 2002) error stop 2
    y = 0
    call drive(e, y)
    if (abs(y - 0.5d0) > 1d-12 .or. count_of(e) /= 1) error stop 3

    y = 0
    call advance(c, y)
    if (abs(y - 5) > 1d-12 .or. peek(c) /= 3003) error stop 4

    p => c
    y = 0
    call advance_pointer(p, y)
    if (abs(y - 5) > 1d-12 .or. c%nsteps /= 4) error stop 5
    select type (x => p)
    type is (TracedEuler)
        if (any(x%log /= [1, 2, 3, 4])) error stop 6
    class default
        error stop 7
    end select

    allocate(o, source=c)
    call advance(o, y)
    if (peek(o) /= 5005 .or. peek(c) /= 4004) error stop 8
    select type (x => o)
    type is (TracedEuler)
        if (any(x%log /= [1, 2, 3, 4, 5])) error stop 9
    class default
        error stop 10
    end select
    deallocate(o)
    print *, "PASS"
end program
