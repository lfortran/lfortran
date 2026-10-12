! Standard-Fortran oracle for traits_associated_02. The program's instances
! of Vector{U} are written out as separate types (U = double precision,
! logical, character(len=5), integer and Vector{integer}) with the bindings
! the program uses and unchanged bodies; Element is U in each instance and
! integer for Tally. Each append keeps its INTENT(INOUT) passed-object
! dummy. Initializers inferred from their argument share the generic name
! Vector, except the character instance, which uses its instance constructor
! like the explicitly named logical and integer instances. := declarations
! become explicit declarations. The checks, their order and the summary line
! match the extension test.
module traits_associated_02_oracle_vector_m
    implicit none
    private
    public :: Vector, Vector_double, Vector_logical, Vector_char5, Vector_integer, &
        Vector_vector

    type :: Vector_double
        private
        double precision, allocatable :: elements(:)
    contains
        procedure, pass :: append => append_double
        procedure, pass :: length => length_double
        procedure, pass :: get => get_double
    end type Vector_double

    type :: Vector_logical
        private
        logical, allocatable :: elements(:)
    contains
        procedure, pass :: append => append_logical
        procedure, pass :: length => length_logical
        procedure, pass :: get => get_logical
    end type Vector_logical

    type :: Vector_char5
        private
        character(len=5), allocatable :: elements(:)
    contains
        procedure, pass :: append => append_char5
        procedure, pass :: length => length_char5
        procedure, pass :: get => get_char5
    end type Vector_char5

    type :: Vector_integer
        private
        integer, allocatable :: elements(:)
    contains
        procedure, pass :: append => append_integer
        procedure, pass :: length => length_integer
        procedure, pass :: get => get_integer
    end type Vector_integer

    type :: Vector_vector
        private
        type(Vector_integer), allocatable :: elements(:)
    contains
        procedure, pass :: append => append_vector
        procedure, pass :: length => length_vector
        procedure, pass :: get => get_vector
    end type Vector_vector

    interface Vector
        module procedure init_double, init_integer, init_vector
    end interface Vector
    interface Vector_logical
        module procedure init_logical
    end interface Vector_logical
    interface Vector_char5
        module procedure init_char5
    end interface Vector_char5
    interface Vector_integer
        module procedure init_integer
    end interface Vector_integer

contains

    function init_double(item) result(res)
        double precision, intent(in) :: item
        type(Vector_double)          :: res
        res%elements = [item]
    end function init_double

    subroutine append_double(self, item)
        class(Vector_double), intent(inout) :: self
        double precision,     intent(in)    :: item
        self%elements = [self%elements, item]
    end subroutine append_double

    function length_double(self) result(n)
        class(Vector_double), intent(in) :: self
        integer                          :: n
        n = size(self%elements)
    end function length_double

    function get_double(self, i) result(item)
        class(Vector_double), intent(in) :: self
        integer,              intent(in) :: i
        double precision                 :: item
        item = self%elements(i)
    end function get_double

    function init_logical(item) result(res)
        logical, intent(in)  :: item
        type(Vector_logical) :: res
        res%elements = [item]
    end function init_logical

    subroutine append_logical(self, item)
        class(Vector_logical), intent(inout) :: self
        logical,               intent(in)    :: item
        self%elements = [self%elements, item]
    end subroutine append_logical

    function length_logical(self) result(n)
        class(Vector_logical), intent(in) :: self
        integer                           :: n
        n = size(self%elements)
    end function length_logical

    function get_logical(self, i) result(item)
        class(Vector_logical), intent(in) :: self
        integer,               intent(in) :: i
        logical                           :: item
        item = self%elements(i)
    end function get_logical

    function init_char5(item) result(res)
        character(len=5), intent(in) :: item
        type(Vector_char5)           :: res
        res%elements = [item]
    end function init_char5

    subroutine append_char5(self, item)
        class(Vector_char5), intent(inout) :: self
        character(len=5),    intent(in)    :: item
        self%elements = [self%elements, item]
    end subroutine append_char5

    function length_char5(self) result(n)
        class(Vector_char5), intent(in) :: self
        integer                         :: n
        n = size(self%elements)
    end function length_char5

    function get_char5(self, i) result(item)
        class(Vector_char5), intent(in) :: self
        integer,             intent(in) :: i
        character(len=5)                :: item
        item = self%elements(i)
    end function get_char5

    function init_integer(item) result(res)
        integer, intent(in)  :: item
        type(Vector_integer) :: res
        res%elements = [item]
    end function init_integer

    subroutine append_integer(self, item)
        class(Vector_integer), intent(inout) :: self
        integer,               intent(in)    :: item
        self%elements = [self%elements, item]
    end subroutine append_integer

    function length_integer(self) result(n)
        class(Vector_integer), intent(in) :: self
        integer                           :: n
        n = size(self%elements)
    end function length_integer

    function get_integer(self, i) result(item)
        class(Vector_integer), intent(in) :: self
        integer,               intent(in) :: i
        integer                           :: item
        item = self%elements(i)
    end function get_integer

    function init_vector(item) result(res)
        type(Vector_integer), intent(in) :: item
        type(Vector_vector)              :: res
        res%elements = [item]
    end function init_vector

    subroutine append_vector(self, item)
        class(Vector_vector), intent(inout) :: self
        type(Vector_integer), intent(in)    :: item
        self%elements = [self%elements, item]
    end subroutine append_vector

    function length_vector(self) result(n)
        class(Vector_vector), intent(in) :: self
        integer                          :: n
        n = size(self%elements)
    end function length_vector

    function get_vector(self, i) result(item)
        class(Vector_vector), intent(in) :: self
        integer,              intent(in) :: i
        type(Vector_integer)             :: item
        item = self%elements(i)
    end function get_vector
end module traits_associated_02_oracle_vector_m

module traits_associated_02_oracle_tally_m
    implicit none
    private

    type, public :: Tally
        private
        integer :: total = 0
        integer :: items = 0
    contains
        procedure, pass :: append => add_item
        procedure, pass :: total_of
        procedure, pass :: count_of
    end type Tally

contains

    subroutine add_item(self, item)
        class(Tally), intent(inout) :: self
        integer,      intent(in)    :: item
        self%total = self%total + item
        self%items = self%items + 1
    end subroutine add_item

    function total_of(self) result(n)
        class(Tally), intent(in) :: self
        integer                  :: n
        n = self%total
    end function total_of

    function count_of(self) result(n)
        class(Tally), intent(in) :: self
        integer                  :: n
        n = self%items
    end function count_of
end module traits_associated_02_oracle_tally_m

module traits_associated_02_oracle_use_m
    use traits_associated_02_oracle_vector_m, only: Vector_integer
    implicit none
    private
    public :: grow

contains

    subroutine grow(v, n)
        type(Vector_integer), intent(inout) :: v
        integer,              intent(in)    :: n
        integer                             :: i
        do i = 1, n
            call v%append(v%length() + 1)
        end do
    end subroutine grow
end module traits_associated_02_oracle_use_m

program traits_associated_02_oracle
    use traits_associated_02_oracle_vector_m, only: Vector, Vector_double, Vector_logical, &
        Vector_char5, Vector_integer, Vector_vector
    use traits_associated_02_oracle_tally_m, only: Tally
    use traits_associated_02_oracle_use_m, only: grow
    implicit none
    integer :: checks, i, total
    type(Vector_integer) :: ints, inner
    type(Vector_double) :: doubles
    type(Vector_logical) :: bools
    type(Vector_char5) :: strings
    type(Vector_vector) :: nested
    type(Tally) :: t

    checks = 0

    doubles = Vector(0.d0)
    call doubles%append(1.5d0)
    call doubles%append(2.2d0)
    call check(doubles%length() == 3, 1)
    call check(doubles%get(1) == 0.d0 .and. doubles%get(2) == 1.5d0 &
        .and. doubles%get(3) == 2.2d0, 2)
    bools = Vector_logical(.true.)
    call bools%append(.false.)
    call bools%append(.true.)
    call check(bools%length() == 3, 3)
    call check(bools%get(1) .and. .not. bools%get(2) .and. bools%get(3), 4)
    strings = Vector_char5("John ")
    call strings%append("Mary ")
    call strings%append("Anne ")
    call check(strings%length() == 3 .and. len(strings%get(3)) == 5, 5)
    call check(strings%get(1) // strings%get(2) // strings%get(3) == "John Mary Anne ", 6)

    ints = Vector_integer(1)
    do i = 2, 25
        call ints%append(i)
    end do
    call grow(ints, 25)
    total = 0
    do i = 1, ints%length()
        total = total + ints%get(i)
    end do
    call check(ints%length() == 50 .and. ints%get(50) == 50, 7)
    call check(total == 1275, 8)

    nested = Vector(Vector(1))
    call nested%append(Vector(2))
    inner = nested%get(2)
    call inner%append(3)
    call check(nested%length() == 2 .and. inner%length() == 2, 9)
    inner = nested%get(2)
    call check(inner%length() == 1 .and. inner%get(1) == 2, 10)

    call t%append(5)
    call t%append(7)
    call check(t%total_of() == 12 .and. t%count_of() == 2, 11)

    print '(a,i0)', 'traits_associated_02 checks: ', checks
    if (checks /= 11) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_02 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_02_oracle
