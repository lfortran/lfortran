module traits_initializers_01_m
    implicit none
    private
    public :: Box, Packet
    type, sealed :: Box
        private
        real :: value = 0.0
    contains
        initial :: init_by_int, init_by_real
        procedure :: get_value
    end type
    type :: Packet
        integer :: n = 2
        integer, allocatable :: values(:)
    contains
        initial :: from_values
    end type
contains
    pure elemental function init_by_int(n, scale) result(object)
        integer, intent(in) :: n
        integer, intent(in), optional :: scale
        type(Box) :: object
        integer :: initial
        initial = 1
        if (present(scale)) initial = scale
        object%value = real(initial * n) + 10.0
    end function
    function init_by_real(a) result(object)
        real, intent(in) :: a
        type(Box) :: object
        object%value = a + 20.0
    end function
    real function get_value(self)
        type(Box), intent(in) :: self
        get_value = self%value
    end function
    function from_values(values) result(object)
        integer, intent(in) :: values(:)
        type(Packet) :: object
        object%n = size(values)
        object%values = values
    end function
end module

program traits_initializers_01
    use traits_initializers_01_m, only: Box, Packet
    implicit none
    type(Box) :: x, y
    type(Box) :: objects(2)
    type(Packet) :: p, q
    x = Box(n=3)
    y = Box(a=4.0)
    if (x%get_value() /= 13.0) error stop 1
    if (y%get_value() /= 24.0) error stop 2
    x = Box(scale=2, n=3)
    if (x%get_value() /= 16.0) error stop 3
    y = Box(1.0)
    if (y%get_value() /= 21.0) error stop 4
    objects = Box([1, 2])
    if (objects(1)%get_value() /= 11.0) error stop 9
    if (objects(2)%get_value() /= 12.0) error stop 10
    p = Packet([1, 2, 3])
    if (p%n /= 3 .or. any(p%values /= [1, 2, 3])) error stop 5
    q = p
    p%values(1) = 9
    if (q%values(1) /= 1) error stop 6
    q = Packet(n=7)
    if (q%n /= 7 .or. allocated(q%values)) error stop 7
    p = Packet()
    if (p%n /= 2 .or. allocated(p%values)) error stop 8
end program
