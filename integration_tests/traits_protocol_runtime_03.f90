! What an open protocol runtime generic message may do with erased values:
! scalar subscripts, triplet sections (reversed, strided, empty), shape
! inquiries, assignments between erased storage, requirements, helpers and
! messages, a BLOCK, and ordinary non-erased controls, intrinsics and
! output. Clients may pass any concrete array expression, including vector
! subscripts.
module traits_protocol_runtime_03_contracts_m
    implicit none
    private
    public :: INumeric, IShape

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface INumeric

    abstract interface :: IShape
        function extent{INumeric :: T}(x) result(n)
            type(T), intent(in) :: x(:, :)
            integer             :: n
        end function extent
        function corners{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:, :)
            type(T)             :: s
        end function corners
    end interface IShape
end module traits_protocol_runtime_03_contracts_m

module traits_protocol_runtime_03_intrinsics_m
    use traits_protocol_runtime_03_contracts_m, only: INumeric
    implicit none
    implements INumeric :: integer
    end implements integer
end module traits_protocol_runtime_03_intrinsics_m

module traits_protocol_runtime_03_provider_m
    use traits_protocol_runtime_03_contracts_m, only: INumeric, IShape
    implicit none
    private
    public :: Grid

    type, sealed, implements(IShape) :: Grid
        logical :: verbose = .false.
    contains
        procedure, pass :: extent => grid_extent
        procedure, pass :: corners => grid_corners
    end type Grid

contains

    function total{INumeric :: U}(x) result(s)
        type(U), intent(in) :: x(:)
        type(U)             :: s
        integer             :: i
        s = U(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function total

    ! Only the shapes of the erased arrays are read.
    function grid_extent{INumeric :: T}(self, x) result(n)
        class(Grid), intent(in) :: self
        type(T),     intent(in) :: x(:, :)
        integer                 :: n
        integer                 :: dims(2), every_other(2)
        dims = shape(x)
        every_other = shape(x(2:, ::2))
        n = 1000 * rank(x) + 100 * dims(1) + 10 * size(x, 2) + &
            ubound(x, 1) - lbound(x, 2) + 10000 * every_other(1) * every_other(2)
        if (self%verbose) print '(a,i0)', 'extent ', n
    end function grid_extent

    function grid_corners{INumeric :: T}(self, x) result(s)
        class(Grid), intent(in) :: self
        type(T),     intent(in) :: x(:, :)
        type(T)                 :: s
        type(T)                 :: first, last
        integer                 :: m, n
        m = size(x, 1)
        n = size(x, 2)
        s = T(0)
        if (m == 0 .or. n == 0) return
        last = x(m, n)
        block
            integer :: row
            row = 1
            first = x(row, 1)
            s = first + last
        end block
        s = s + total(x(m:1:-1, 1)) + total(x(1, n:1:-2)) + total(x(2:1, 1))
        if (self%verbose) print '(a,i0,a,i0)', 'corners of ', m, ' x ', n
    end function grid_corners
end module traits_protocol_runtime_03_provider_m

program traits_protocol_runtime_03
    use traits_protocol_runtime_03_contracts_m, only: IShape
    use traits_protocol_runtime_03_intrinsics_m
    use traits_protocol_runtime_03_provider_m, only: Grid
    implicit none
    class(IShape), allocatable :: g
    integer :: a(3, 4), b(0:1, -1:2), i, checks

    checks = 0
    g = Grid()
    a = reshape([(i, i = 1, 12)], [3, 4])
    b = reshape([(10 * i, i = 1, 8)], [2, 4])

    call check(g%extent(a) == 42342, 1)
    call check(g%extent(b) == 22241, 2)
    call check(g%extent(a(:, [4, 1])) == 22322, 3)
    ! 1 + 12, column 1 reversed 3+2+1, row 1 at columns 4, 2: 10 + 4
    call check(g%corners(a) == 33, 4)
    call check(g%corners(a([3, 1], 2:3) + 1) == 37, 5)
    call check(g%corners(a(:, 2:1)) == 0, 6)
    call check(g%corners(b) == 220, 7)

    print '(a,i0)', 'traits_protocol_runtime_03 checks: ', checks
    if (checks /= 7) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_protocol_runtime_03 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_protocol_runtime_03
