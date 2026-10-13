module traits_runtime_owning_13_m
    implicit none
    integer :: events = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Root
        integer :: value = 17
    contains
        final :: root_scalar
        final :: root_vector
        final :: root_matrix
    end type
    type :: Leaf
        integer :: digit = 3
        integer, allocatable :: data(:)
    contains
        final :: leaf_final
    end type
    type, extends(Root) :: Parent
        integer :: tag = 5
        type(Leaf), allocatable :: own_leaf
    contains
        final :: parent_vector
        final :: parent_matrix
    end type
    type, extends(Parent) :: Child
        integer :: padding(3) = 9
        type(Leaf), allocatable :: child_leaf
    contains
        final :: child_final
    end type
    type :: VectorHolder
        type(Child), allocatable :: parts(:)
    end type
    type :: MatrixHolder
        type(Child), allocatable :: parts(:, :)
    end type
    implements IValue :: VectorHolder
        procedure, pass :: value => vector_size
    end implements
    implements IValue :: MatrixHolder
        procedure, pass :: value => matrix_size
    end implements
contains
    function vector_size(self) result(r)
        class(VectorHolder), intent(in) :: self
        integer :: r
        r = size(self%parts)
    end function
    function matrix_size(self) result(r)
        class(MatrixHolder), intent(in) :: self
        integer :: r
        r = size(self%parts)
    end function
    subroutine root_scalar(self)
        type(Root), intent(inout) :: self
        error stop 20
    end subroutine
    subroutine root_vector(self)
        type(Root), intent(inout) :: self(:)
        if (events /= 2233144 .or. size(self) /= 2) error stop 21
        if (sum(self%value) /= -2) error stop 22
        events = 10 * events + 5
    end subroutine
    subroutine root_matrix(self)
        type(Root), intent(inout) :: self(:, :)
        if (events /= 2233644 .or. any(shape(self) /= [2, 1])) error stop 23
        if (sum(self%value) /= -2) error stop 24
        events = 10 * events + 7
    end subroutine
    subroutine parent_vector(self)
        type(Parent), intent(inout) :: self(:)
        if (events /= 2233 .or. size(self) /= 2) error stop 31
        if (sum(self%value) /= 34 .or. sum(self%tag) /= 10) error stop 32
        self%value = -1
        events = 10 * events + 1
    end subroutine
    subroutine parent_matrix(self)
        type(Parent), intent(inout) :: self(:, :)
        if (events /= 2233 .or. any(shape(self) /= [2, 1])) error stop 33
        if (sum(self%value) /= 34 .or. sum(self%tag) /= 10) error stop 34
        self%value = -1
        events = 10 * events + 6
    end subroutine
    impure elemental subroutine child_final(self)
        type(Child), intent(inout) :: self
        if (any(self%padding /= 9)) error stop 40
        events = 10 * events + 2
    end subroutine
    impure elemental subroutine leaf_final(self)
        type(Leaf), intent(inout) :: self
        if (any(self%data /= 5)) error stop 41
        events = 10 * events + self%digit
    end subroutine
    subroutine initialize(value)
        type(Child), intent(inout) :: value
        allocate(value%own_leaf, value%child_leaf)
        value%own_leaf%digit = 4
        allocate(value%own_leaf%data(3), value%child_leaf%data(2))
        value%own_leaf%data = 5
        value%child_leaf%data = 5
    end subroutine
end module

program traits_runtime_owning_13
    use traits_runtime_owning_13_m
    implicit none
    type(VectorHolder) :: vector_source
    type(MatrixHolder) :: matrix_source
    class(IValue), allocatable :: vector, matrix
    integer :: i

    allocate(vector_source%parts(2), matrix_source%parts(2, 1))
    do i = 1, 2
        call initialize(vector_source%parts(i))
        call initialize(matrix_source%parts(i, 1))
    end do
    allocate(vector, source=vector_source)
    allocate(matrix, source=matrix_source)
    if (events /= 0) error stop 1

    vector = vector_source
    if (events /= 22331445) error stop 2
    if (vector%value() /= 2) error stop 10
    if (sum(vector_source%parts%value) /= 34) error stop 3
    events = 0
    matrix = matrix_source
    if (events /= 22336447) error stop 4
    if (matrix%value() /= 2) error stop 11
    if (sum(matrix_source%parts%value) /= 34) error stop 5
    events = 0
    deallocate(vector)
    if (events /= 22331445) error stop 6
    events = 0
    deallocate(matrix)
    if (events /= 22336447) error stop 7
    events = 0
    deallocate(vector_source%parts)
    if (events /= 22331445) error stop 8
    events = 0
    deallocate(matrix_source%parts)
    if (events /= 22336447) error stop 9
end program
