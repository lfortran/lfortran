module traits_runtime_inspection_nested_01_types
    implicit none
    type :: Parent
        integer :: n
    end type
    type, extends(Parent) :: Child
        integer :: extra
    end type
    type, extends(Child) :: Grandchild
        integer :: last
    end type
    type :: Other
        integer :: n
    end type
end module

module traits_runtime_inspection_nested_01_m
    use traits_runtime_inspection_nested_01_types, only: ParentAlias => Parent, Child, Grandchild, Other
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    implements IValue :: Grandchild
        procedure, pass :: value => get
    end implements
contains
    integer function get(self)
        type(Grandchild), intent(in) :: self
        get = self%n
    end function
    integer function inspect(view) result(r)
        class(IValue), intent(in) :: view
        r = 0
        select type (outer => view)
        type is (Other)
            error stop 1
        class is (ParentAlias)
            select type (middle => outer)
            type is (ParentAlias)
                error stop 2
            class is (Child)
                r = middle%n + middle%extra
                select type (inner => middle)
                type is (Grandchild)
                    r = r + inner%last
                class default
                    error stop 3
                end select
            class is (ParentAlias)
                error stop 4
            end select
        class default
            error stop 5
        end select
    end function
    subroutine mutate(view)
        class(IValue), pointer, intent(in) :: view
        select type (outer => view)
        class is (ParentAlias)
            select type (inner => outer)
            class is (Grandchild)
                inner%n = inner%n + 1
            class default
                error stop 6
            end select
        end select
    end subroutine
end module

program traits_runtime_inspection_nested_01
    use traits_runtime_inspection_nested_01_m
    implicit none
    type(Grandchild), target :: object
    class(IValue), pointer :: view
    object%n = 47
    object%extra = 11
    object%last = 13
    if (inspect(object) /= 71 .or. object%n /= 47) error stop 7
    view => object
    call mutate(view)
    if (inspect(view) /= 72 .or. object%n /= 48) error stop 8
    nullify(view)
end program
