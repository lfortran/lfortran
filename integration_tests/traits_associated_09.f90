module traits_associated_09_m
    implicit none
    abstract interface :: ISubject
        type :: ObsType
        subroutine registerObserver(self, obs)
            type(itself), pass, intent(inout) :: self
            type(ObsType), intent(in) :: obs
        end subroutine
        function observer(self) result(obs)
            type(itself), pass, intent(in) :: self
            type(ObsType) :: obs
        end function
    end interface
    abstract interface :: IItem
    end interface
    type, sealed, implements(IItem) :: Item
        integer :: value
    end type
    type, sealed, implements(ISubject) :: Subject
        integer :: last = 0
    contains
        procedure :: registerObserver
        procedure :: observer
    end type
    type, sealed, implements(ISubject) :: ObjectSubject
        integer :: last = 0
    contains
        procedure :: registerObserver => register_object
        procedure :: observer => object_observer
    end type
    type, sealed :: View{ISubject(ObsType=Item) :: S}
        type(S) :: data
    contains
        procedure :: read
    end type
contains
    function read(self) result(obs)
        type(View{S}), intent(in) :: self
        type(Item) :: obs
        obs = self%data%observer()
    end function
    function observer(self) result(obs)
        type(Subject), intent(in) :: self
        integer :: obs
        obs = self%last
    end function
    function object_observer(self) result(obs)
        type(ObjectSubject), intent(in) :: self
        type(Item) :: obs
        obs%value = self%last
    end function
    subroutine register_object(self, obs)
        type(ObjectSubject), intent(inout) :: self
        type(Item), intent(in) :: obs
        self%last = obs%value
    end subroutine
    subroutine registerObserver(self, obs)
        type(Subject), intent(inout) :: self
        integer, intent(in) :: obs
        self%last = obs
    end subroutine
    subroutine put_object{ISubject(ObsType=O) :: S, IItem :: O}(data, value)
        type(S), intent(inout) :: data
        type(O), intent(in) :: value
        call data%registerObserver(value)
    end subroutine
    subroutine put{ISubject(ObsType=integer) :: T}(data, value)
        type(T), intent(inout) :: data
        integer, intent(in) :: value
        call data%registerObserver(value)
        if (data%observer() /= value) error stop 3
    end subroutine
    subroutine forward{ISubject(ObsType=integer) :: U}(data)
        type(U), intent(inout) :: data
        call put(data, 42)
    end subroutine
end module
program traits_associated_09
    use traits_associated_09_m
    implicit none
    type(Subject) :: data
    type(ObjectSubject) :: object
    type(View{ObjectSubject}) :: window
    type(Item) :: copy
    call forward(data)
    if (data%last /= 42) error stop 1
    call put_object(object, Item(43))
    if (object%last /= 43) error stop 2
    window%data%last = 44
    copy = window%read()
    if (copy%value /= 44) error stop 4
end program
