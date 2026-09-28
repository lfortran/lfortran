! Derived types accessed through a namespace: declarations, structure
! constructors, components, type-bound procedures, polymorphism.
module namespace_modules_04_geo
    implicit none
    type :: point_t
        real :: x = 0, y = 0
    contains
        procedure :: shift
        procedure :: norm2 => point_norm2
    end type

    type, extends(point_t) :: point3_t
        real :: z = 0
    end type

    type(point_t) :: origin = point_t(0.0, 0.0)
contains
    subroutine shift(self, dx, dy)
        class(point_t), intent(inout) :: self
        real, intent(in) :: dx, dy
        self%x = self%x + dx
        self%y = self%y + dy
    end subroutine

    real function point_norm2(self)
        class(point_t), intent(in) :: self
        point_norm2 = self%x**2 + self%y**2
    end function
end module

program namespace_modules_04
    use, namespace :: geo => namespace_modules_04_geo
    implicit none
    type(geo%point_t) :: p
    type(geo%point_t), allocatable :: arr(:)
    class(geo%point_t), allocatable :: c
    type(geo%point3_t) :: q

    ! Default initialization and structure constructors
    if (abs(p%x) > 1e-6 .or. abs(p%y) > 1e-6) error stop
    p = geo%point_t(3.0, 4.0)
    if (abs(p%norm2() - 25.0) > 1e-5) error stop
    p = geo%point_t(y=1.0, x=2.0)
    if (abs(p%x - 2.0) > 1e-6 .or. abs(p%y - 1.0) > 1e-6) error stop

    ! Type-bound procedure call
    call p%shift(1.0, 1.0)
    if (abs(p%x - 3.0) > 1e-6 .or. abs(p%y - 2.0) > 1e-6) error stop

    ! Module variable of derived type: components and bindings through the
    ! namespace
    geo%origin%x = 6.0
    geo%origin%y = 8.0
    if (abs(geo%origin%norm2() - 100.0) > 1e-4) error stop
    call geo%origin%shift(-6.0, -8.0)
    if (abs(geo%origin%norm2()) > 1e-6) error stop

    ! Array constructor with a type-spec
    arr = [geo%point_t :: geo%point_t(1.0, 0.0), geo%point_t(0.0, 2.0)]
    if (size(arr) /= 2) error stop
    if (abs(arr(2)%norm2() - 4.0) > 1e-6) error stop

    ! Polymorphism: allocate with a type-spec, select type
    q = geo%point3_t(1.0, 2.0, 3.0)
    allocate(geo%point3_t :: c)
    select type (c)
    type is (geo%point3_t)
        c%z = 5.0
    class default
        error stop
    end select
    c = q
    call check(c)

    print *, p%x, p%y, q%z
contains
    subroutine check(v)
        class(geo%point_t), intent(in) :: v
        select type (v)
        type is (geo%point_t)
            error stop
        class is (geo%point3_t)
            if (abs(v%z - 3.0) > 1e-6) error stop
        class default
            error stop
        end select
    end subroutine
end program
