! Standard-Fortran oracle for traits_protocol_02. The constrained generic
! procedures are written out for each type argument the program uses (Cell,
! real and real(real64)) with unchanged bodies; T(1) becomes Cell(1), REAL(1)
! or REAL(1, real64). Cell's initializer is a generic interface named like
! the type and its static elemental operators are a generic operator
! interface. The checks, their order and the summary line match the
! extension test.
module traits_protocol_02_oracle_cell_m
    implicit none
    private
    public :: Cell, value_of, operator(*)

    type :: Cell
        private
        integer :: n = 0
    end type Cell

    interface Cell
        module procedure init
    end interface Cell

    interface operator(*)
        module procedure multiply, multiply_by_int
    end interface operator(*)

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
end module traits_protocol_02_oracle_cell_m

module traits_protocol_02_oracle_generic_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_02_oracle_cell_m, only: Cell, operator(*)
    implicit none
    private
    public :: prod, squares, scaled, weighted, products4, grid_product, triple

    interface prod
        module procedure prod_cell, prod_real, prod_real64
    end interface prod
    interface squares
        module procedure squares_cell, squares_real, squares_real64
    end interface squares
    interface scaled
        module procedure scaled_cell, scaled_real, scaled_real64
    end interface scaled
    interface weighted
        module procedure weighted_cell, weighted_real, weighted_real64
    end interface weighted
    interface products4
        module procedure products4_cell, products4_real, products4_real64
    end interface products4
    interface grid_product
        module procedure grid_product_cell, grid_product_real
    end interface grid_product
    interface triple
        module procedure triple_cell, triple_real, triple_real64
    end interface triple

contains

    function prod_cell(arr) result(res)
        type(Cell), intent(in) :: arr(:)
        type(Cell)             :: res
        integer                :: i
        res = Cell(1)
        do i = 1, size(arr)
            res = res * arr(i)
        end do
    end function prod_cell

    function prod_real(arr) result(res)
        real, intent(in) :: arr(:)
        real             :: res
        integer          :: i
        res = real(1)
        do i = 1, size(arr)
            res = res * arr(i)
        end do
    end function prod_real

    function prod_real64(arr) result(res)
        real(real64), intent(in) :: arr(:)
        real(real64)             :: res
        integer                  :: i
        res = real(1, real64)
        do i = 1, size(arr)
            res = res * arr(i)
        end do
    end function prod_real64

    function squares_cell(a) result(r)
        type(Cell), intent(in) :: a(:)
        type(Cell)             :: r(size(a))
        r = a * a
    end function squares_cell

    function squares_real(a) result(r)
        real, intent(in) :: a(:)
        real             :: r(size(a))
        r = a * a
    end function squares_real

    function squares_real64(a) result(r)
        real(real64), intent(in) :: a(:)
        real(real64)             :: r(size(a))
        r = a * a
    end function squares_real64

    function scaled_cell(a, s, k) result(r)
        type(Cell), intent(in) :: a(:)
        type(Cell), intent(in) :: s
        integer,    intent(in) :: k
        type(Cell)             :: r(size(a))
        r = (a * s) * k
    end function scaled_cell

    function scaled_real(a, s, k) result(r)
        real,    intent(in) :: a(:)
        real,    intent(in) :: s
        integer, intent(in) :: k
        real                :: r(size(a))
        r = (a * s) * k
    end function scaled_real

    function scaled_real64(a, s, k) result(r)
        real(real64), intent(in) :: a(:)
        real(real64), intent(in) :: s
        integer,      intent(in) :: k
        real(real64)             :: r(size(a))
        r = (a * s) * k
    end function scaled_real64

    function weighted_cell(a, w) result(r)
        type(Cell), intent(in) :: a(:)
        integer,    intent(in) :: w(:)
        type(Cell)             :: r
        r = prod((a * a) * w)
    end function weighted_cell

    function weighted_real(a, w) result(r)
        real,    intent(in) :: a(:)
        integer, intent(in) :: w(:)
        real                :: r
        r = prod((a * a) * w)
    end function weighted_real

    function weighted_real64(a, w) result(r)
        real(real64), intent(in) :: a(:)
        integer,      intent(in) :: w(:)
        real(real64)             :: r
        r = prod((a * a) * w)
    end function weighted_real64

    function products4_cell(at) result(r)
        type(Cell), intent(in) :: at(4)
        type(Cell)             :: r
        r = prod( (at * at) * [4,3,2,1] )
    end function products4_cell

    function products4_real(at) result(r)
        real, intent(in) :: at(4)
        real             :: r
        r = prod( (at * at) * [4,3,2,1] )
    end function products4_real

    function products4_real64(at) result(r)
        real(real64), intent(in) :: at(4)
        real(real64)             :: r
        r = prod( (at * at) * [4,3,2,1] )
    end function products4_real64

    function grid_product_cell(m) result(r)
        type(Cell), intent(in) :: m(:,:)
        type(Cell)             :: r
        type(Cell)             :: sq(size(m, 1), size(m, 2))
        integer                :: j
        sq = m * m
        r = Cell(1)
        do j = 1, size(m, 2)
            r = r * prod(sq(:, j))
        end do
    end function grid_product_cell

    function grid_product_real(m) result(r)
        real, intent(in) :: m(:,:)
        real             :: r
        real             :: sq(size(m, 1), size(m, 2))
        integer          :: j
        sq = m * m
        r = real(1)
        do j = 1, size(m, 2)
            r = r * prod(sq(:, j))
        end do
    end function grid_product_real

    function triple_cell(x) result(r)
        type(Cell), intent(in) :: x
        type(Cell)             :: r
        r = prod([x, x * 2, x * 3])
    end function triple_cell

    function triple_real(x) result(r)
        real, intent(in) :: x
        real             :: r
        r = prod([x, x * 2, x * 3])
    end function triple_real

    function triple_real64(x) result(r)
        real(real64), intent(in) :: x
        real(real64)             :: r
        r = prod([x, x * 2, x * 3])
    end function triple_real64
end module traits_protocol_02_oracle_generic_m

program traits_protocol_02_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_02_oracle_cell_m, only: Cell, value_of
    use traits_protocol_02_oracle_generic_m, only: prod, squares, scaled, weighted, &
        products4, grid_product, triple
    implicit none
    integer :: checks, i
    type(Cell) :: c(4), cm(2, 2), cr
    real :: r(4), rm(2, 2)
    real(real64) :: d(4)

    checks = 0
    c = [(Cell(i), i = 1, 4)]
    r = [(real(i), i = 1, 4)]
    d = [(real(i, real64), i = 1, 4)]

    cr = products4(c)
    call check(value_of(cr) == 13824, 1)
    call check(products4(r) == 13824.0, 2)
    call check(products4(d) == 13824.0_real64, 3)
    cr = weighted(c, [4, 3, 2, 1])
    call check(value_of(cr) == 13824, 4)
    call check(weighted(r, [4, 3, 2, 1]) == 13824.0, 5)
    call check(weighted(d, [4, 3, 2, 1]) == 13824.0_real64, 6)

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

    call check(all(value_of(scaled(c, Cell(2), 3)) == [6, 12, 18, 24]), 16)
    call check(all(scaled(r, 2.0, 3) == [6.0, 12.0, 18.0, 24.0]), 17)
    call check(all(scaled(d(2:3), 0.5_real64, -2) == [-2.0_real64, -3.0_real64]), 18)
    cr = weighted(c(2:3), [5, 7])
    call check(value_of(cr) == 1260, 19)
    call check(weighted(d(2:3), [5, 7]) == 1260.0_real64, 20)

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
end program traits_protocol_02_oracle
