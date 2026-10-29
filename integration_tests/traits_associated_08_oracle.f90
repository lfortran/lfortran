! Standard-Fortran specializations of traits_associated_08.
module traits_associated_08_oracle_m
    implicit none
    type :: Subject
        integer :: last = 0
    contains
        procedure :: registerObserver => register_integer
        procedure :: observer => integer_observer
    end type
    type :: RealSubject
        real :: last = 0
    contains
        procedure :: registerObserver => register_real
        procedure :: observer => real_observer
    end type
    type :: Display_IR
        type(Subject) :: subject
        type(RealSubject) :: other
    contains
        procedure :: refresh => refresh_ir
    end type
    type :: Observer
        integer :: value = 0
    end type
    type :: ObjectSubject
        integer :: last = 0
    contains
        procedure :: registerObserver => register_object
        procedure :: observer => object_observer
    end type
    type :: Display_OI
        type(ObjectSubject) :: subject
        type(Subject) :: other
    contains
        procedure :: refresh => refresh_oi
    end type
    type :: Display_RI
        type(RealSubject) :: subject
        type(Subject) :: other
    contains
        procedure :: refresh => refresh_ri
    end type
    interface forward
        module procedure forward_integer, forward_real, forward_object
    end interface
contains
    subroutine register_integer(self, obs)
        class(Subject), intent(inout) :: self
        integer, intent(in) :: obs
        self%last = obs
    end subroutine
    function integer_observer(self) result(obs)
        class(Subject), intent(in) :: self
        integer :: obs
        obs = self%last + 1
    end function
    subroutine register_real(self, obs)
        class(RealSubject), intent(inout) :: self
        real, intent(in) :: obs
        self%last = obs
    end subroutine
    function real_observer(self) result(obs)
        class(RealSubject), intent(in) :: self
        real :: obs
        obs = self%last + 0.5
    end function
    subroutine refresh_ir(self)
        class(Display_IR), intent(inout) :: self
        call self%subject%registerObserver(self%subject%observer())
        call self%other%registerObserver(self%other%observer())
    end subroutine
    subroutine register_object(self, obs)
        class(ObjectSubject), intent(inout) :: self
        type(Observer), intent(in) :: obs
        self%last = obs%value
    end subroutine
    function object_observer(self) result(obs)
        class(ObjectSubject), intent(in) :: self
        type(Observer) :: obs
        obs%value = self%last + 2
    end function
    subroutine refresh_oi(self)
        class(Display_OI), intent(inout) :: self
        call self%subject%registerObserver(self%subject%observer())
        call self%other%registerObserver(self%other%observer())
    end subroutine
    subroutine refresh_ri(self)
        class(Display_RI), intent(inout) :: self
        call self%subject%registerObserver(self%subject%observer())
        call self%other%registerObserver(self%other%observer())
    end subroutine
    subroutine forward_integer(item)
        type(Subject), intent(inout) :: item
        call item%registerObserver(item%observer())
    end subroutine
    subroutine forward_real(item)
        type(RealSubject), intent(inout) :: item
        call item%registerObserver(item%observer())
    end subroutine
    subroutine forward_object(item)
        type(ObjectSubject), intent(inout) :: item
        call item%registerObserver(item%observer())
    end subroutine
end module
program traits_associated_08_oracle
    use traits_associated_08_oracle_m
    implicit none
    type(Display_IR) :: view
    type(Display_RI) :: reversed
    type(Display_OI) :: objects
    call view%subject%registerObserver(42)
    call view%other%registerObserver(1.5)
    if (view%subject%last /= 42) error stop 1
    call view%refresh()
    if (view%subject%last /= 43) error stop 2
    if (view%other%last /= 2.0) error stop 3
    call reversed%refresh()
    if (reversed%subject%last /= 0.5) error stop 4
    if (reversed%other%last /= 1) error stop 5
    call forward(view%subject)
    call forward(view%other)
    if (view%subject%last /= 44) error stop 6
    if (view%other%last /= 2.5) error stop 7
    call objects%subject%registerObserver(Observer(40))
    call objects%refresh()
    if (objects%subject%last /= 42) error stop 8
    call forward(objects%subject)
    if (objects%subject%last /= 44) error stop 9
end program
