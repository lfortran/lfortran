module traits_associated_09_oracle_m
    implicit none
    type :: Item
        integer :: value
    end type
    type :: ObjectSubject
        integer :: last = 0
    contains
        procedure :: registerObserver => register_object
    end type
    type :: Subject
        integer :: last = 0
    contains
        procedure :: registerObserver
        procedure :: observer
    end type
    type :: View
        type(ObjectSubject) :: data
    contains
        procedure :: read
    end type
contains
    function read(self) result(obs)
        class(View), intent(in) :: self
        type(Item) :: obs
        obs%value = self%data%last
    end function
    subroutine register_object(self, obs)
        class(ObjectSubject), intent(inout) :: self
        type(Item), intent(in) :: obs
        self%last = obs%value
    end subroutine
    subroutine put_object(data, value)
        type(ObjectSubject), intent(inout) :: data
        type(Item), intent(in) :: value
        call data%registerObserver(value)
    end subroutine
    function observer(self) result(obs)
        class(Subject), intent(in) :: self
        integer :: obs
        obs = self%last
    end function
    subroutine registerObserver(self, obs)
        class(Subject), intent(inout) :: self
        integer, intent(in) :: obs
        self%last = obs
    end subroutine
    subroutine put(data, value)
        type(Subject), intent(inout) :: data
        integer, intent(in) :: value
        call data%registerObserver(value)
        if (data%observer() /= value) error stop 3
    end subroutine
    subroutine forward(data)
        type(Subject), intent(inout) :: data
        call put(data, 42)
    end subroutine
end module
program traits_associated_09_oracle
    use traits_associated_09_oracle_m
    implicit none
    type(Subject) :: data
    type(ObjectSubject) :: object
    type(View) :: window
    type(Item) :: copy
    call forward(data)
    if (data%last /= 42) error stop 1
    call put_object(object, Item(43))
    if (object%last /= 43) error stop 2
    window%data%last = 44
    copy = window%read()
    if (copy%value /= 44) error stop 4
end program
