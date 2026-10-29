module traits_associated_10_oracle_m
    implicit none
    type :: Display
        type(Subject), pointer :: subject
        integer :: value
    end type
    type :: Subject
        integer :: last = 0
    contains
        procedure :: registerObserver
    end type
contains
    function init(data) result(self)
        type(Subject), target, intent(inout) :: data
        type(Display) :: self
        self%subject => data
        self%value = 42
        call data%registerObserver(self)
    end function
    subroutine registerObserver(self, obs)
        class(Subject), intent(inout) :: self
        type(Display), intent(in) :: obs
        self%last = obs%value
    end subroutine
end module
program traits_associated_10_oracle
    use traits_associated_10_oracle_m
    implicit none
    type(Subject), target :: data
    type(Display) :: observer
    observer = init(data)
    if (data%last /= 42) error stop 1
    if (.not. associated(observer%subject, data)) error stop 2
    observer%subject%last = 43
    if (data%last /= 43) error stop 3
end program
