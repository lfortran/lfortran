module traits_generic_type_09_oracle_m
    implicit none
    type :: Subject
        integer :: value = 0
    end type
    type :: Display
        type(Subject), pointer :: weatherData
    end type
    interface Display
        module procedure init
    end interface
    type :: Box
        integer, pointer :: item
    end type
    type :: ArrayBox
        integer, pointer :: items(:)
    end type
    interface Box
        module procedure box_init
    end interface
contains
    function init(weatherData) result(self)
        type(Subject), target, intent(inout) :: weatherData
        type(Display) :: self
        self%weatherData => weatherData
    end function

    function box_init(item) result(self)
        integer, target, intent(inout) :: item
        type(Box) :: self
        self%item => item
    end function

    subroutine redirect(p, x)
        type(Subject), pointer, intent(inout) :: p
        type(Subject), target, intent(inout) :: x
        p => x
    end subroutine
end module

program traits_generic_type_09_oracle
    use traits_generic_type_09_oracle_m
    implicit none
    type(Subject), target :: data, other
    type(Display) :: disp, inferred
    type(Box) :: box_i
    type(ArrayBox) :: box_a
    integer, target :: n = 7
    integer, target :: values(2) = [1, 2]

    nullify(disp%weatherData, box_i%item)
    if (associated(disp%weatherData)) error stop 1
    if (associated(box_i%item)) error stop 2
    data%value = 42
    disp = Display(data)
    if (.not. associated(disp%weatherData, data)) error stop 3
    if (disp%weatherData%value /= 42) error stop 4
    disp%weatherData%value = 43
    if (data%value /= 43) error stop 5

    inferred = Display(other)
    other%value = 8
    if (.not. associated(inferred%weatherData, other)) error stop 6
    if (inferred%weatherData%value /= 8) error stop 7
    call redirect(disp%weatherData, other)
    if (.not. associated(disp%weatherData, other)) error stop 8
    nullify(disp%weatherData)
    if (associated(disp%weatherData)) error stop 9
    if (.not. associated(inferred%weatherData, other)) error stop 10

    box_i = Box(n)
    if (.not. associated(box_i%item, n)) error stop 11
    box_i%item = 9
    if (n /= 9) error stop 12

    box_a%items => values
    if (.not. associated(box_a%items, values)) error stop 13
    if (size(box_a%items) /= 2) error stop 14
    box_a%items(2) = 4
    if (values(2) /= 4) error stop 15
end program
