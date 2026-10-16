! A derived type with allocatable scalar components -- a deferred-length
! character, a real and a derived type, also inside a component -- that a
! do concurrent loop reads only other components of: through a type-bound
! procedure that is passed the whole value, and through component
! references of elements of an array of the type. The device copy of the
! value keeps the host's bytes for those components, so the components
! after them are read where the host put them, and they are unchanged
! after the loop.
module gpu_metal_429_m
    implicit none
    type :: string_t
        character(len=:), allocatable :: string_
    end type
    type :: inner_t
        integer :: id
    end type
    type :: net_t
        character(len=:), allocatable :: name
        integer :: offset
        real, allocatable :: scale
        type(string_t) :: labels(2)
        type(inner_t), allocatable :: inner
        integer, allocatable :: nodes(:)
        integer :: factor
    contains
        procedure :: first_node
        procedure :: apply
    end type
contains
    elemental integer function first_node(self)
        class(net_t), intent(in) :: self
        first_node = self%nodes(1) * self%factor
    end function

    subroutine apply(self, x)
        class(net_t), intent(in) :: self
        real, intent(out) :: x(:)
        integer :: i
        do concurrent (i = 1:size(x))
            x(i) = real(self%first_node() * i + self%offset)
        end do
    end subroutine
end module

program gpu_metal_429
    use gpu_metal_429_m
    implicit none
    type(net_t) :: net, nets(3)
    real :: x(4)
    integer :: y(3), i

    net%name = "network"
    net%offset = 7
    net%scale = 2.5
    net%labels(1)%string_ = "inputs"
    net%labels(2)%string_ = "outputs"
    allocate(net%inner)
    net%inner%id = 11
    allocate(net%nodes(2))
    net%nodes = [3, 5]
    net%factor = 2

    call net%apply(x)
    if (any(abs(x - [13., 19., 25., 31.]) > 1e-6)) error stop 1

    do i = 1, 3
        nets(i)%name = repeat("n", i)
        nets(i)%offset = 10 * i
        nets(i)%factor = i
    end do
    do concurrent (i = 1:3)
        y(i) = nets(i)%offset + nets(i)%factor
        nets(i)%factor = 2 * nets(i)%factor
    end do
    if (any(y /= [11, 22, 33])) error stop 2
    if (any(nets%factor /= [2, 4, 6])) error stop 3

    if (net%name /= "network") error stop 4
    if (abs(net%scale - 2.5) > 1e-6) error stop 5
    if (net%labels(1)%string_ /= "inputs") error stop 6
    if (net%labels(2)%string_ /= "outputs") error stop 7
    if (net%inner%id /= 11) error stop 8
    do i = 1, 3
        if (nets(i)%name /= repeat("n", i)) error stop 9
    end do
    print *, x, y
end program
