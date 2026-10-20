! The provider of a container and of a problem whose registration stores
! factories through a view of the container.
module traits_procarg_sep_01_oracle_provider_m
    use traits_procarg_sep_01_oracle_contracts_m, only: IResolver, &
        IContainer, IProblem, ifactory, Mesh
    implicit none
    private
    public :: Container, Problem

    type :: Entry
        character(len=:), allocatable :: key
        procedure(ifactory), pointer, nopass :: factory => null()
    end type Entry

    type, extends(IContainer) :: Container
        type(Entry) :: entries(8)
        integer :: n = 0
        integer :: missing = 0
    contains
        procedure :: resolve => container_resolve
        procedure :: register => container_register
    end type Container

    type, extends(IProblem) :: Problem
        integer :: cells = 0
    contains
        procedure :: registration => problem_registration
    end type Problem
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
        integer :: i
        if (.not. associated(factory)) then
            self%missing = self%missing + 1
            return
        end if
        do i = 1, self%n
            if (self%entries(i)%key == key) then
                self%entries(i)%factory => factory
                return
            end if
        end do
        if (self%n == size(self%entries)) error stop 40
        self%n = self%n + 1
        self%entries(self%n)%key = key
        self%entries(self%n)%factory => factory
    end subroutine container_register

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
            error stop 41
        end select
        select type (width)
        type is (real(8))
            m%width = width
        class default
            error stop 42
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
end module traits_procarg_sep_01_oracle_provider_m
