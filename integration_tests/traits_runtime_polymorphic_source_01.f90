! Borrowed trait views formed from polymorphic class(Euler) objects keep the
! dynamic type of their payload: an override in the extension TracedEuler
! runs through the view, SELECT TYPE on the view sees TracedEuler, and
! messages with intent(inout) receivers modify the original object. A
! binding of a retroactive conformance that is not type-bound (euler_label)
! calls Euler's procedure, and an abstract declared type (Stepper) dispatches
! through its deferred binding.
module traits_runtime_polymorphic_source_01_m
    implicit none
    abstract interface :: IStep
        subroutine step(self, y)
            type(itself), intent(inout), pass :: self
            real(8), intent(inout) :: y
        end subroutine
        integer function count(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: ILabel
        integer function label(self)
            type(itself), intent(in), pass :: self
        end function
    end interface

    type, implements(IStep) :: Euler
        real(8) :: h = 0.5d0
        integer :: nsteps = 0
    contains
        procedure :: step => euler_step
        procedure :: count => euler_count
        procedure :: run => euler_run
    end type

    type, extends(Euler) :: TracedEuler
        integer :: traced = 0
    contains
        procedure :: step => traced_step
        procedure :: count => traced_count
    end type

    implements ILabel :: Euler
        procedure, pass :: label => euler_label
    end implements Euler

    type, abstract, implements(IStep) :: Stepper
        integer :: nsteps = 0
    contains
        procedure(stepper_step), deferred :: step
        procedure :: count => stepper_count
    end type
    abstract interface
        subroutine stepper_step(self, y)
            import :: Stepper
            class(Stepper), intent(inout) :: self
            real(8), intent(inout) :: y
        end subroutine
    end interface

    type, extends(Stepper) :: Doubler
    contains
        procedure :: step => doubler_step
    end type

    type :: Holder
        class(Euler), allocatable :: solver
        class(Euler), pointer :: link => null()
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
        y = y + 10*self%h
        self%nsteps = self%nsteps + 1
        self%traced = self%traced + 1
    end subroutine

    integer function traced_count(self)
        class(TracedEuler), intent(in) :: self
        traced_count = 1000*self%traced + self%nsteps
    end function

    integer function euler_label(self)
        class(Euler), intent(in) :: self
        euler_label = 100 + self%nsteps
    end function

    integer function stepper_count(self)
        class(Stepper), intent(in) :: self
        stepper_count = self%nsteps
    end function

    subroutine doubler_step(self, y)
        class(Doubler), intent(inout) :: self
        real(8), intent(inout) :: y
        y = 2*y
        self%nsteps = self%nsteps + 1
    end subroutine

    ! Consumers of the contracts.
    subroutine advance(s, y)
        class(IStep), intent(inout) :: s
        real(8), intent(inout) :: y
        call s%step(y)
    end subroutine

    subroutine forward(s, y)
        class(IStep), intent(inout) :: s
        real(8), intent(inout) :: y
        call advance(s, y)
    end subroutine

    integer function peek(s)
        class(IStep), intent(in) :: s
        peek = s%count()
    end function

    integer function kind_of(s)
        class(IStep), intent(in) :: s
        select type (concrete => s)
        type is (TracedEuler)
            kind_of = 2
        type is (Euler)
            kind_of = 1
        class is (Stepper)
            kind_of = 3
        class default
            kind_of = 0
        end select
    end function

    integer function label_of(x)
        class(ILabel), intent(in) :: x
        label_of = x%label()
    end function

    ! Views of polymorphic objects.
    subroutine euler_run(self, y)
        class(Euler), intent(inout) :: self
        real(8), intent(inout) :: y
        call advance(self, y)
    end subroutine

    subroutine drive(e, y)
        class(Euler), intent(inout) :: e
        real(8), intent(inout) :: y
        call forward(e, y)
    end subroutine

    integer function count_of(e)
        class(Euler), intent(in) :: e
        count_of = peek(e)
    end function

    integer function kind_via(e)
        class(Euler), intent(in) :: e
        kind_via = kind_of(e)
    end function

    integer function label_via(e)
        class(Euler), intent(in) :: e
        label_via = label_of(e)
    end function
end module

program traits_runtime_polymorphic_source_01
    use traits_runtime_polymorphic_source_01_m
    implicit none
    type(TracedEuler), target :: t
    type(Euler) :: e
    class(Euler), allocatable :: c
    class(Euler), allocatable :: arr(:)
    class(IStep), allocatable :: o
    class(Stepper), allocatable :: st
    type(Holder) :: h
    real(8) :: y

    ! A class(Euler) dummy whose dynamic type is TracedEuler.
    y = 1
    call drive(t, y)
    if (abs(y - 6) > 1d-12) error stop 1
    if (t%nsteps /= 1 .or. t%traced /= 1) error stop 2
    y = 1
    call drive(e, y)
    if (abs(y - 1.5d0) > 1d-12 .or. e%nsteps /= 1) error stop 3
    if (kind_via(t) /= 2 .or. kind_via(e) /= 1) error stop 4
    if (count_of(t) /= 1001 .or. count_of(e) /= 1) error stop 5

    ! The passed object of a type-bound procedure.
    y = 0
    call t%run(y)
    if (abs(y - 5) > 1d-12 .or. t%traced /= 2 .or. t%nsteps /= 2) error stop 6
    y = 0
    call e%run(y)
    if (abs(y - 0.5d0) > 1d-12 .or. e%nsteps /= 2) error stop 7

    ! A binding that is not type-bound is not overridden.
    if (label_via(t) /= 102 .or. label_via(e) /= 102) error stop 8

    ! An allocatable class(Euler) variable.
    allocate(TracedEuler :: c)
    y = 0
    call advance(c, y)
    if (abs(y - 5) > 1d-12 .or. count_of(c) /= 1001 .or. kind_via(c) /= 2) error stop 9

    ! CLASS IS associates of an owner and of an ordinary class object.
    allocate(TracedEuler :: o)
    select type (x => o)
    class is (Euler)
        y = 0
        call advance(x, y)
        if (abs(y - 5) > 1d-12) error stop 10
        if (kind_of(x) /= 2 .or. peek(x) /= 1001) error stop 11
    end select
    if (peek(o) /= 1001) error stop 12
    select type (x => c)
    class is (Euler)
        y = 0
        call forward(x, y)
        if (abs(y - 5) > 1d-12) error stop 13
    end select
    if (count_of(c) /= 2002) error stop 14

    ! Components and array elements.
    allocate(TracedEuler :: h%solver)
    y = 0
    call advance(h%solver, y)
    if (abs(y - 5) > 1d-12 .or. h%solver%nsteps /= 1) error stop 15
    if (peek(h%solver) /= 1001 .or. kind_of(h%solver) /= 2) error stop 16
    h%link => t
    y = 0
    call advance(h%link, y)
    if (abs(y - 5) > 1d-12 .or. t%traced /= 3 .or. t%nsteps /= 3) error stop 17
    if (peek(h%link) /= 3003) error stop 18
    allocate(TracedEuler :: arr(2))
    y = 0
    call advance(arr(2), y)
    if (abs(y - 5) > 1d-12) error stop 19
    if (arr(1)%nsteps /= 0 .or. arr(2)%nsteps /= 1) error stop 20
    if (peek(arr(2)) /= 1001 .or. kind_of(arr(1)) /= 2) error stop 21

    ! An abstract declared type dispatches through its deferred binding.
    allocate(Doubler :: st)
    y = 3
    call advance(st, y)
    if (abs(y - 6) > 1d-12 .or. peek(st) /= 1 .or. kind_of(st) /= 3) error stop 22
    print *, "PASS"
end program
