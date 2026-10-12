! Elemental operator requirements of a trait applied to arrays of a
! constrained type (paper sect:overloading): whole arrays, array-valued
! generic results, strided, reversed and empty sections, scalar broadcasts,
! integer array factors, rank-2 arrays, generic array constructors and the
! explicit-shape dummy of the paper's products. The derived type supplies
! static elemental operators through a retroactive implements block; real
! and real(real64) are acknowledged and use the intrinsic elemental
! multiplication, also by integer.
module traits_protocol_02_contracts_m
    implicit none
    private
    public :: IReducible

    abstract interface :: IReducible
        initial(n)
            integer, intent(in) :: n
        end initial
        elemental function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
        elemental function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs
            integer,      intent(in) :: rhs
            type(itself)             :: res
        end function operator(*)
    end interface IReducible
end module traits_protocol_02_contracts_m

module traits_protocol_02_cell_m
    use traits_protocol_02_contracts_m, only: IReducible
    implicit none
    private
    public :: value_of

    type, public, sealed :: Cell
        private
        integer :: n = 0
    end type Cell

    implements IReducible :: Cell
        initial :: init
        procedure, static :: operator(*) => multiply, multiply_by_int
    end implements Cell

contains

    function init(n) result(res)
        integer, intent(in) :: n
        type(Cell)          :: res
        res%n = n
    end function init

    elemental function multiply(lhs, rhs) result(res)
        type(Cell), intent(in) :: lhs, rhs
        type(Cell)             :: res
        res%n = lhs%n * rhs%n
    end function multiply

    elemental function multiply_by_int(lhs, rhs) result(res)
        type(Cell), intent(in) :: lhs
        integer,    intent(in) :: rhs
        type(Cell)             :: res
        res%n = lhs%n * rhs
    end function multiply_by_int

    elemental function value_of(c) result(v)
        type(Cell), intent(in) :: c
        integer                :: v
        v = c%n
    end function value_of
end module traits_protocol_02_cell_m

module traits_protocol_02_intrinsics_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_02_contracts_m, only: IReducible
    implicit none

    implements IReducible :: real
    end implements real

    implements IReducible :: real(real64)
    end implements real(real64)
end module traits_protocol_02_intrinsics_m

module traits_protocol_02_generic_m
    use traits_protocol_02_contracts_m, only: IReducible
    implicit none
    private
    public :: prod, squares, scaled, weighted, products4, grid_product, triple

contains

    function prod{IReducible :: T}(arr) result(res)
        type(T), intent(in) :: arr(:)
        type(T)             :: res
        integer             :: i
        res = T(1)
        do i = 1, size(arr)
            res = res * arr(i)
        end do
    end function prod

    ! An array-valued result of the elemental itself * itself operation.
    function squares{IReducible :: T}(a) result(r)
        type(T), intent(in) :: a(:)
        type(T)             :: r(size(a))
        r = a * a
    end function squares

    ! A scalar broadcast of itself * itself, then itself * integer.
    function scaled{IReducible :: T}(a, s, k) result(r)
        type(T), intent(in) :: a(:)
        type(T), intent(in) :: s
        integer, intent(in) :: k
        type(T)             :: r(size(a))
        r = (a * s) * k
    end function scaled

    function weighted{IReducible :: T}(a, w) result(r)
        type(T), intent(in) :: a(:)
        integer, intent(in) :: w(:)
        type(T)             :: r
        r = prod((a * a) * w)
    end function weighted

    function products4{IReducible :: T}(at) result(r)
        type(T), intent(in) :: at(4)
        type(T)             :: r
        r = prod( (at * at) * [4,3,2,1] )
    end function products4

    function grid_product{IReducible :: T}(m) result(r)
        type(T), intent(in) :: m(:,:)
        type(T)             :: r
        type(T)             :: sq(size(m, 1), size(m, 2))
        integer             :: j
        sq = m * m
        r = T(1)
        do j = 1, size(m, 2)
            r = r * prod(sq(:, j))
        end do
    end function grid_product

    function triple{IReducible :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = prod([x, x * 2, x * 3])
    end function triple
end module traits_protocol_02_generic_m

program traits_protocol_02
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_02_cell_m, only: Cell, value_of
    use traits_protocol_02_intrinsics_m
    use traits_protocol_02_generic_m, only: prod, squares, scaled, weighted, products4, &
        grid_product, triple
    implicit none
    integer :: checks, i
    type(Cell) :: c(4), cm(2, 2), cr
    real :: r(4), rm(2, 2)
    real(real64) :: d(4)

    checks = 0
    c = [(Cell(i), i = 1, 4)]
    r = [(real(i), i = 1, 4)]
    d = [(real(i, real64), i = 1, 4)]

    ! The paper's reduction for the derived type, real and real(real64).
    cr = products4(c)
    call check(value_of(cr) == 13824, 1)
    call check(products4(r) == 13824.0, 2)
    call check(products4(d) == 13824.0_real64, 3)
    cr = weighted(c, [4, 3, 2, 1])
    call check(value_of(cr) == 13824, 4)
    call check(weighted(r, [4, 3, 2, 1]) == 13824.0, 5)
    call check(weighted(d, [4, 3, 2, 1]) == 13824.0_real64, 6)

    ! Array-valued generic results of whole arrays and sections.
    call check(all(value_of(squares(c)) == [1, 4, 9, 16]), 7)
    call check(all(squares(r) == [1.0, 4.0, 9.0, 16.0]), 8)
    call check(all(squares(d) == [1.0_real64, 4.0_real64, 9.0_real64, 16.0_real64]), 9)
    call check(all(value_of(squares(c(1:4:2))) == [1, 9]), 10)
    call check(all(value_of(squares(c(4:1:-1))) == [16, 9, 4, 1]), 11)
    call check(all(squares(r(4:1:-2)) == [16.0, 4.0]), 12)
    call check(size(squares(c(1:0))) == 0, 13)
    cr = prod(c(1:0))
    call check(value_of(cr) == 1, 14)
    call check(prod(d(3:2)) == 1.0_real64, 15)

    ! Scalar broadcasts and integer factors.
    call check(all(value_of(scaled(c, Cell(2), 3)) == [6, 12, 18, 24]), 16)
    call check(all(scaled(r, 2.0, 3) == [6.0, 12.0, 18.0, 24.0]), 17)
    call check(all(scaled(d(2:3), 0.5_real64, -2) == [-2.0_real64, -3.0_real64]), 18)
    cr = weighted(c(2:3), [5, 7])
    call check(value_of(cr) == 1260, 19)
    call check(weighted(d(2:3), [5, 7]) == 1260.0_real64, 20)

    ! Rank-2 arrays and generic array constructors.
    cm = reshape(c, [2, 2])
    rm = reshape(r, [2, 2])
    cr = grid_product(cm)
    call check(value_of(cr) == 576, 21)
    call check(grid_product(rm) == 576.0, 22)
    cr = triple(Cell(2))
    call check(value_of(cr) == 48, 23)
    call check(triple(2.0) == 48.0, 24)
    call check(triple(0.5_real64) == 0.75_real64, 25)

    print '(a,i0)', 'traits_protocol_02 checks: ', checks
    if (checks /= 25) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_protocol_02 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_protocol_02
