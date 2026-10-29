! Associated types of a constraint are dependent on its implementing binder,
! not additional user-supplied generic arguments.
module traits_associated_08_m
    implicit none
    abstract interface :: ISubject
        type :: ObsType
        subroutine registerObserver(self, obs)
            type(itself), intent(inout), pass :: self
            type(ObsType), intent(in) :: obs
        end subroutine
        function observer(self) result(obs)
            type(itself), intent(in), pass :: self
            type(ObsType) :: obs
        end function
    end interface
    type, sealed, implements(ISubject) :: Subject
        integer :: last = 0
    contains
        procedure :: registerObserver => register_integer
        procedure :: observer => integer_observer
    end type
    type, sealed, implements(ISubject) :: RealSubject
        real :: last = 0
    contains
        procedure :: registerObserver => register_real
        procedure :: observer => real_observer
    end type
    type :: Observer
        integer :: value = 0
    end type
    type, sealed, implements(ISubject) :: ObjectSubject
        integer :: last = 0
    contains
        procedure :: registerObserver => register_object
        procedure :: observer => object_observer
    end type
    type :: Display{ISubject :: U, ISubject :: V}
        type(U) :: subject
        type(V) :: other
    contains
        procedure :: refresh
    end type
contains
    subroutine register_integer(self, obs)
        type(Subject), intent(inout) :: self
        integer, intent(in) :: obs
        self%last = obs
    end subroutine
    function integer_observer(self) result(obs)
        type(Subject), intent(in) :: self
        integer :: obs
        obs = self%last + 1
    end function
    subroutine register_real(self, obs)
        type(RealSubject), intent(inout) :: self
        real, intent(in) :: obs
        self%last = obs
    end subroutine
    function real_observer(self) result(obs)
        type(RealSubject), intent(in) :: self
        real :: obs
        obs = self%last + 0.5
    end function
    subroutine register_object(self, obs)
        type(ObjectSubject), intent(inout) :: self
        type(Observer), intent(in) :: obs
        self%last = obs%value
    end subroutine
    function object_observer(self) result(obs)
        type(ObjectSubject), intent(in) :: self
        type(Observer) :: obs
        obs%value = self%last + 2
    end function
    subroutine refresh(self)
        class(Display{U,V}), intent(inout) :: self
        call self%subject%registerObserver(self%subject%observer())
        call self%other%registerObserver(self%other%observer())
    end subroutine
    subroutine advance{ISubject :: T}(subject)
        type(T), intent(inout) :: subject
        call subject%registerObserver(subject%observer())
    end subroutine
    subroutine forward{ISubject :: T}(subject)
        type(T), intent(inout) :: subject
        call advance(subject)
    end subroutine
end module
program traits_associated_08
    use traits_associated_08_m
    implicit none
    type(Display{Subject,RealSubject}) :: view
    type(Display{RealSubject,Subject}) :: reversed
    type(Display{ObjectSubject,Subject}) :: objects
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
