module traits_associated_10_contract
    implicit none
    abstract interface :: ISubject
        type :: ObsType
        subroutine registerObserver(self, obs)
            type(itself), pass, intent(inout) :: self
            type(ObsType), intent(in) :: obs
        end subroutine
    end interface
    type :: Display{ISubject(ObsType=Display{U}) :: U}
        type(U), pointer :: subject
        integer :: value
    contains
        initial :: init
    end type
contains
    function init(data) result(self)
        type(U), target, intent(inout) :: data
        type(Display{U}) :: self
        self%subject => data
        self%value = 42
        call data%registerObserver(self)
    end function
end module
module traits_associated_10_subject
    use traits_associated_10_contract
    implicit none
    type, sealed, implements(ISubject) :: Subject
        integer :: last = 0
    contains
        procedure :: registerObserver
    end type
contains
    subroutine registerObserver(self, obs)
        type(Subject), intent(inout) :: self
        type(Display{Subject}), intent(in) :: obs
        self%last = obs%value
    end subroutine
end module
program traits_associated_10
    use traits_associated_10_subject
    implicit none
    type(Subject), target :: data
    type(Display{Subject}) :: observer
    observer = Display{Subject}(data)
    if (data%last /= 42) error stop 1
    if (.not. associated(observer%subject, data)) error stop 2
    observer%subject%last = 43
    if (data%last /= 43) error stop 3
end program
