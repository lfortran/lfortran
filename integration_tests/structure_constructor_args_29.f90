! A type name in an array constructor that is the value of an array component
! refers to the generic interface that overloads the type's structure
! constructor whenever the interface accepts the arguments (F2018 7.5.10,
! NOTE 1), so the private components are set by the specific procedure.
module structure_constructor_args_29_m
    implicit none
    private
    public :: item_t, holder_t, get

    type :: item_t
        private
        integer :: a
    end type

    interface item_t
        procedure :: make_item
    end interface

    type :: holder_t
        type(item_t) :: items(2)
    end type

contains

    function make_item(a) result(r)
        integer, intent(in) :: a
        type(item_t) :: r
        r%a = 2*a
    end function

    integer function get(x)
        type(item_t), intent(in) :: x
        get = x%a
    end function

end module

program structure_constructor_args_29
    use structure_constructor_args_29_m
    implicit none
    type(holder_t) :: h

    h = holder_t(items=[item_t(4), item_t(a=5)])
    if (get(h%items(1)) /= 8 .or. get(h%items(2)) /= 10) error stop 1

    h = holder_t([item_t(1), item_t(2)])
    if (get(h%items(1)) /= 2 .or. get(h%items(2)) /= 4) error stop 2

    h = holder_t([item_t :: item_t(3), item_t(a=4)])
    if (get(h%items(1)) /= 6 .or. get(h%items(2)) /= 8) error stop 3

    print *, get(h%items(1)), get(h%items(2))
end program
