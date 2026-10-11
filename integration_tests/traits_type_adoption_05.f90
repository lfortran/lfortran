module traits_type_adoption_05_m
    implicit none
    integer :: receiver_finals = 0, receiver_sum = 0
    integer :: token_finals = 0, token_sum = 0
    type :: Token
        integer :: n = 0
    contains
        final :: finish_token
    end type
    type :: Parent
        integer :: n = 17
    contains
        procedure, pass(self) :: reset => parent_reset
    end type
    type, extends(Parent), sealed :: Closed
    contains
        procedure, pass(self) :: reset => closed_reset
        final :: finish_closed
    end type
contains
    subroutine finish_token(self)
        type(Token), intent(inout) :: self
        token_finals = token_finals + 1
        token_sum = token_sum + self%n
    end subroutine

    subroutine finish_closed(self)
        type(Closed), intent(inout) :: self
        receiver_finals = receiver_finals + 1
        receiver_sum = receiver_sum + self%n
    end subroutine

    subroutine parent_reset(n, self, item, array, values)
        integer, intent(in) :: n
        class(Parent), intent(out) :: self
        type(Token), intent(out) :: item
        integer, allocatable, intent(out) :: array(:)
        real, intent(in) :: values(n)
        self%n = -1
        item%n = -1
        allocate(array(n))
        array = -1
    end subroutine

    subroutine closed_reset(n, self, item, array, values)
        integer, intent(in) :: n
        type(Closed), intent(out) :: self
        type(Token), intent(out) :: item
        integer, allocatable, intent(out) :: array(:)
        real, intent(in) :: values(n)
        if (allocated(array)) error stop 1
        self%n = n + int(sum(values))
        item%n = 9
        allocate(array(n))
        array = self%n
    end subroutine

    subroutine reset_through_parent(self, item, array)
        class(Parent), intent(inout) :: self
        type(Token), intent(inout) :: item
        integer, allocatable, intent(inout) :: array(:)
        call self%reset(values=[3.0, 4.0], array=array, item=item, n=2)
    end subroutine
end module

program traits_type_adoption_05
    use traits_type_adoption_05_m
    implicit none
    type(Closed), target :: object
    type(Token) :: item
    class(Parent), pointer :: ancestor
    integer, allocatable :: array(:)
    object%n = 77
    item%n = 88
    allocate(array(4))
    array = -1
    ancestor => object
    call reset_through_parent(ancestor, item, array)
    if (receiver_finals /= 1 .or. receiver_sum /= 77) error stop 2
    if (token_finals /= 1 .or. token_sum /= 88) error stop 3
    if (object%n /= 9 .or. item%n /= 9) error stop 4
    if (size(array) /= 2 .or. any(array /= 9)) error stop 5
    deallocate(array)
    nullify(ancestor)
end program
