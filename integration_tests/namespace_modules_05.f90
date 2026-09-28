! Type extension of a type accessed through a namespace. The parent
! component is named after the parent type's name in the module (base_t).
module namespace_modules_05_shapes
    implicit none
    type, abstract :: shape_t
        character(len=8) :: label = "shape"
    contains
        procedure(area_iface), deferred :: area
        procedure :: describe
    end type

    abstract interface
        real function area_iface(self)
            import :: shape_t
            class(shape_t), intent(in) :: self
        end function
    end interface

    type :: base_t
        integer :: id = 0
    end type
contains
    integer function describe(self)
        class(shape_t), intent(in) :: self
        describe = nint(self%area())
    end function
end module

module namespace_modules_05_user
    use, namespace :: shp => namespace_modules_05_shapes
    implicit none
    type, extends(shp%shape_t) :: square_t
        real :: side = 1
    contains
        procedure :: area => square_area
    end type

    type, extends(shp%base_t) :: derived_t
        integer :: extra = 0
    end type
contains
    real function square_area(self)
        class(square_t), intent(in) :: self
        square_area = self%side**2
    end function
end module

program namespace_modules_05
    use namespace_modules_05_user
    implicit none
    type(square_t) :: s
    type(derived_t) :: d

    s%side = 3
    if (abs(s%area() - 9.0) > 1e-6) error stop
    if (s%describe() /= 9) error stop
    if (s%label /= "shape") error stop

    d%id = 4
    d%extra = 5
    ! Parent component keeps the parent type's name
    if (d%base_t%id /= 4) error stop
    if (d%id + d%extra /= 9) error stop
    print *, s%area(), d%id, d%extra
end program
