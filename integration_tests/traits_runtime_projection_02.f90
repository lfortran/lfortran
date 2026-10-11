module traits_runtime_projection_02_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface, extends(IValue) :: IStaticChild
        real function fraction()
        end function
    end interface
    abstract interface, extends(IValue) :: IForeignChild
        integer function foreign_value()
        end function
    end interface
    type :: Box
        integer :: n = 13
    end type
    type, bind(c) :: ForeignBox
        integer :: n = 29
    end type
    implements IStaticChild :: Box
        procedure, pass :: value => box_value
        procedure, pass :: fraction => box_fraction
    end implements
    implements IForeignChild :: ForeignBox
        procedure, pass :: value => foreign_box_value
        procedure, nopass :: foreign_value => foreign_value
    end implements
contains
    integer function box_value(self)
        type(Box), intent(in) :: self
        box_value = self%n
    end function
    real function box_fraction(self)
        type(Box), intent(in) :: self
        box_fraction = self%n / 2.0
    end function
    integer function foreign_box_value(self)
        type(ForeignBox), intent(in) :: self
        foreign_box_value = self%n
    end function
    integer function foreign_value() bind(c)
        foreign_value = 41
    end function
    integer function observe(view)
        class(IValue), intent(in) :: view
        observe = view%value()
    end function
end module

program traits_runtime_projection_02
    use traits_runtime_projection_02_m
    implicit none
    type(Box), target :: source
    type(ForeignBox), target :: foreign
    class(IValue), pointer :: view
    class(IValue), allocatable :: owner
    if (observe(source) /= 13 .or. observe(foreign) /= 29) error stop 1
    view => source
    if (view%value() /= 13) error stop 2
    view => foreign
    if (view%value() /= 29) error stop 3
    nullify(view)
    allocate(owner, source=source)
    if (owner%value() /= 13) error stop 4
    owner = foreign
    if (owner%value() /= 29) error stop 5
    deallocate(owner)
end program
