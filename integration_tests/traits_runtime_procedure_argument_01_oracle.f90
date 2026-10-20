! Standard-Fortran counterpart of traits_runtime_procedure_argument_01.f90:
! the traits are emulated with abstract types and deferred bindings.
module traits_runtime_procedure_argument_01_oracle_contracts
    implicit none
    type, abstract :: IResolver
    contains
        procedure(resolve_interface), deferred :: resolve
    end type IResolver

    abstract interface
        function resolve_interface(self, key) result(res)
            import; implicit none
            class(IResolver), intent(in) :: self
            character(*), intent(in) :: key
            class(*), allocatable :: res
        end function resolve_interface
    end interface

    abstract interface
        function ifactory(c) result(res)
            import; implicit none
            class(IResolver), intent(in) :: c
            class(*), allocatable :: res
        end function ifactory
    end interface

    type, abstract, extends(IResolver) :: IContainer
    contains
        procedure(register_interface), deferred :: register
    end type IContainer

    abstract interface
        subroutine register_interface(self, key, factory)
            import; implicit none
            class(IContainer), intent(inout) :: self
            character(*), intent(in) :: key
            procedure(ifactory), pointer, intent(in) :: factory
        end subroutine register_interface
    end interface

    type, abstract :: IProblem
    contains
        procedure(registration_interface), deferred :: registration
    end type IProblem

    abstract interface
        subroutine registration_interface(self, c)
            import; implicit none
            class(IProblem), intent(in) :: self
            class(IContainer), intent(inout) :: c
        end subroutine registration_interface
    end interface
end module traits_runtime_procedure_argument_01_oracle_contracts

module traits_runtime_procedure_argument_01_oracle_container
    use traits_runtime_procedure_argument_01_oracle_contracts, only: IResolver, IContainer, ifactory
    implicit none
    private
    public :: Container

    type :: Entry
        character(len=:), allocatable :: key
        procedure(ifactory), pointer, nopass :: factory => null()
    end type Entry

    type, extends(IContainer) :: Container
        type(Entry), allocatable :: entries(:)
        integer :: n = 0
        integer :: missing = 0
    contains
        procedure :: resolve => container_resolve
        procedure :: register => container_register
    end type Container
contains
    function container_resolve(self, key) result(res)
        class(Container), intent(in) :: self
        character(*), intent(in) :: key
        class(*), allocatable :: res
        integer :: i
        do i = 1, self%n
            if (self%entries(i)%key == key) then
                res = self%entries(i)%factory(self)
                return
            end if
        end do
        res = -1
    end function container_resolve

    subroutine container_register(self, key, factory)
        class(Container), intent(inout) :: self
        character(*), intent(in) :: key
        procedure(ifactory), pointer, intent(in) :: factory
        type(Entry), allocatable :: grown(:)
        if (.not. associated(factory)) then
            self%missing = self%missing + 1
            return
        end if
        if (.not. allocated(self%entries)) allocate(self%entries(4))
        if (self%n == size(self%entries)) then
            allocate(grown(2 * self%n))
            grown(1:self%n) = self%entries
            call move_alloc(grown, self%entries)
        end if
        self%n = self%n + 1
        self%entries(self%n)%key = key
        self%entries(self%n)%factory => factory
    end subroutine container_register
end module traits_runtime_procedure_argument_01_oracle_container

module traits_runtime_procedure_argument_01_oracle_problem
    use traits_runtime_procedure_argument_01_oracle_contracts, only: IResolver, IContainer, &
        IProblem, ifactory
    implicit none
    private
    public :: Problem, Mesh

    type :: Mesh
        integer :: cells = 0
        real(8) :: width = 0
    end type Mesh

    type, extends(IProblem) :: Problem
        integer :: size = 0
    contains
        procedure :: registration => problem_registration
    end type Problem
contains
    function make_count(c) result(res)
        class(IResolver), intent(in) :: c
        class(*), allocatable :: res
        res = 42
    end function make_count

    function make_width(c) result(res)
        class(IResolver), intent(in) :: c
        class(*), allocatable :: res
        res = 2.5d0
    end function make_width

    function make_name(c) result(res)
        class(IResolver), intent(in) :: c
        class(*), allocatable :: res
        res = "heat equation"
    end function make_name

    ! Builds a mesh from the other registrations of the container.
    function make_mesh(c) result(res)
        class(IResolver), intent(in) :: c
        class(*), allocatable :: res
        class(*), allocatable :: count, width
        type(Mesh) :: m
        count = c%resolve("count")
        width = c%resolve("width")
        select type (count)
        type is (integer)
            m%cells = count
        class default
            error stop 1
        end select
        select type (width)
        type is (real(8))
            m%width = width
        class default
            error stop 2
        end select
        res = m
    end function make_mesh

    subroutine problem_registration(self, c)
        class(Problem), intent(in) :: self
        class(IContainer), intent(inout) :: c
        procedure(ifactory), pointer :: f
        f => make_count
        call c%register("count", f)
        f => make_width
        call c%register("width", f)
        f => make_name
        call c%register("name", f)
        f => make_mesh
        call c%register("mesh", f)
        f => null()
        call c%register("nothing", f)
    end subroutine problem_registration
end module traits_runtime_procedure_argument_01_oracle_problem

module traits_runtime_procedure_argument_01_oracle_checks
    use traits_runtime_procedure_argument_01_oracle_contracts, only: IResolver, IContainer, ifactory
    use traits_runtime_procedure_argument_01_oracle_problem, only: Mesh
    implicit none
contains
    subroutine check_all(c)
        class(IResolver), intent(in) :: c
        class(*), allocatable :: r
        r = c%resolve("count")
        select type (r)
        type is (integer)
            if (r /= 42) error stop 10
        class default
            error stop 11
        end select
        r = c%resolve("width")
        select type (r)
        type is (real(8))
            if (abs(r - 2.5d0) > 1d-12) error stop 12
        class default
            error stop 13
        end select
        r = c%resolve("name")
        select type (r)
        type is (character(*))
            if (r /= "heat equation") error stop 14
        class default
            error stop 15
        end select
        r = c%resolve("mesh")
        select type (r)
        type is (Mesh)
            if (r%cells /= 42) error stop 16
            if (abs(r%width - 2.5d0) > 1d-12) error stop 17
        class default
            error stop 18
        end select
        r = c%resolve("unknown")
        select type (r)
        type is (integer)
            if (r /= -1) error stop 19
        class default
            error stop 20
        end select
    end subroutine check_all

    ! Registers one more factory through a borrowed view of the container.
    subroutine add_answer(c)
        class(IContainer), intent(inout) :: c
        procedure(ifactory), pointer :: f
        f => answer
        call c%register("answer", f)
    end subroutine add_answer

    function answer(c) result(res)
        class(IResolver), intent(in) :: c
        class(*), allocatable :: res
        class(*), allocatable :: count
        count = c%resolve("count")
        select type (count)
        type is (integer)
            res = count + 1
        class default
            res = 0
        end select
    end function answer
end module traits_runtime_procedure_argument_01_oracle_checks

program traits_runtime_procedure_argument_01_oracle
    use traits_runtime_procedure_argument_01_oracle_contracts, only: IContainer, IProblem, IResolver
    use traits_runtime_procedure_argument_01_oracle_container, only: Container
    use traits_runtime_procedure_argument_01_oracle_problem, only: Problem
    use traits_runtime_procedure_argument_01_oracle_checks, only: check_all, add_answer
    implicit none
    class(IContainer), allocatable :: c
    class(IProblem), allocatable :: p
    class(IContainer), pointer :: cp
    type(Container), target :: direct
    class(*), allocatable :: r

    allocate(Container :: c)
    allocate(Problem :: p)
    call p%registration(c)
    call check_all(c)
    call add_answer(c)
    r = c%resolve("answer")
    select type (r)
    type is (integer)
        if (r /= 43) error stop 30
    class default
        error stop 31
    end select

    ! The same problem registers into a concrete container through a view
    ! and a trait pointer.
    call p%registration(direct)
    if (direct%n /= 4 .or. direct%missing /= 1) error stop 32
    cp => direct
    call check_all(cp)
    call add_answer(cp)
    if (direct%n /= 5) error stop 33
    r = cp%resolve("answer")
    select type (r)
    type is (integer)
        if (r /= 43) error stop 34
    class default
        error stop 35
    end select
    select type (c)
    type is (Container)
        if (c%n /= 5 .or. c%missing /= 1) error stop 36
    class default
        error stop 37
    end select
    print *, "ok"
end program traits_runtime_procedure_argument_01_oracle
