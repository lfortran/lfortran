program derived_type_shape_pointer_01
    implicit none
    type t
        real, pointer :: a(:)
    end type t
    type(t) :: x
    integer :: s(1)

    allocate(x%a(3))
    x%a = [1.0, 2.0, 3.0]

    s = shape(x%a)
    if (s(1) /= 3) error stop
    print *, s(1)

    deallocate(x%a)
end program derived_type_shape_pointer_01
