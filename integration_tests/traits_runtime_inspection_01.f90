module traits_runtime_inspection_01_m
    use traits_runtime_inspection_twin_a_m, only: LeftTwin => Twin
    use traits_runtime_inspection_twin_b_m, only: RightTwin => Twin
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface, extends(IValue) :: IChild
    end interface
    type :: Root
        integer :: payload
    end type
    type, extends(Root) :: Branch
        integer :: extra
    end type
    type, extends(Branch) :: Leaf
        integer :: more
    end type
    type :: Unrelated
        integer :: payload
    end type
    implements IValue :: LeftTwin
        procedure, pass :: value => left_value
    end implements
    implements IValue :: RightTwin
        procedure, pass :: value => right_value
    end implements
    implements IValue :: Leaf
        procedure, pass :: value => leaf_value
    end implements
    implements IChild :: Unrelated
        procedure, pass :: value => unrelated_value
    end implements
contains
    integer function left_value(self)
        type(LeftTwin), intent(in) :: self
        left_value = self%payload
    end function
    integer function right_value(self)
        type(RightTwin), intent(in) :: self
        right_value = self%payload
    end function
    integer function leaf_value(self)
        type(Leaf), intent(in) :: self
        leaf_value = self%payload
    end function
    integer function unrelated_value(self)
        type(Unrelated), intent(in) :: self
        unrelated_value = self%payload
    end function
    integer function identity(object)
        class(IValue), intent(in) :: object
        select type (concrete => object)
        class is (Root)
            identity = 33
        type is (LeftTwin)
            identity = 11
        type is (RightTwin)
            identity = 22
        type is (Root)
            identity = 31
        type is (Branch)
            identity = 32
        class default
            identity = 90
            if (concrete%value() /= object%value()) error stop 1
        end select
    end function
    integer function most_specific(object)
        class(IValue), intent(in) :: object
        most_specific = -1
        select type (object)
        class is (Root)
            error stop 2
        class is (Branch)
            most_specific = object%payload + object%extra
        end select
    end function
    integer function exact_leaf(object)
        class(IValue), intent(in) :: object
        select type (concrete => object)
        class is (Root)
            error stop 3
        class is (Branch)
            error stop 4
        type is (Leaf)
            exact_leaf = concrete%more
        class default
            exact_leaf = -1
        end select
    end function
end module

program traits_runtime_inspection_01
    use traits_runtime_inspection_01_m
    implicit none
    type(LeftTwin), target :: left
    type(RightTwin), target :: right
    type(Leaf), target :: leaf_object
    type(Unrelated), target :: outside
    class(IValue), pointer :: view => null()
    class(IChild), pointer :: child_view => null()
    integer :: i, expected
    left%payload = 41
    right%payload = 41
    if (storage_size(left) /= storage_size(right)) error stop 5
    do i = 0, 1
        if (mod(command_argument_count() + i, 2) == 0) then
            view => left
            expected = 11
        else
            view => right
            expected = 22
        end if
        if (identity(view) /= expected) error stop 6
        if (most_specific(view) /= -1 .or. exact_leaf(view) /= -1) error stop 7
        if (view%value() /= 41) error stop 8
    end do
    leaf_object%payload = 71
    leaf_object%extra = 2
    leaf_object%more = 3
    view => leaf_object
    if (identity(view) /= 33) error stop 9
    if (most_specific(view) /= 73 .or. exact_leaf(view) /= 3) error stop 10
    if (view%value() /= 71) error stop 11
    outside%payload = 81
    child_view => outside
    view => child_view
    if (identity(view) /= 90) error stop 12
    if (most_specific(view) /= -1 .or. exact_leaf(view) /= -1) error stop 13
    if (view%value() /= 81 .or. child_view%value() /= 81) error stop 14
    nullify(view, child_view)
end program
