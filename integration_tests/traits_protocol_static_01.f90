! A protocol whose operator(*) takes an array of type(itself) has no
! runtime dictionaries, but its conformances and generic procedures keep
! working statically.
module traits_protocol_static_01_contracts_m
    implicit none
    private
    public :: IAccum

    abstract interface :: IAccum
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
        function operator(*)(lhs, v) result(res)
            type(itself), intent(in) :: lhs
            type(itself), intent(in) :: v(:)
            type(itself)             :: res
        end function operator(*)
    end interface IAccum
end module traits_protocol_static_01_contracts_m

module traits_protocol_static_01_cell_m
    use traits_protocol_static_01_contracts_m, only: IAccum
    implicit none
    private
    type, public, sealed :: Cell
        integer :: n = 0
    end type Cell

    implements IAccum :: Cell
        initial :: make
        procedure, static :: operator(+) => add
        procedure, static :: operator(*) => total
    end implements Cell
contains
    function make(n) result(res)
        integer, intent(in) :: n
        type(Cell) :: res
        res%n = n
    end function make

    function add(lhs, rhs) result(res)
        type(Cell), intent(in) :: lhs, rhs
        type(Cell) :: res
        res%n = lhs%n + rhs%n
    end function add

    function total(lhs, v) result(res)
        type(Cell), intent(in) :: lhs
        type(Cell), intent(in) :: v(:)
        type(Cell) :: res
        integer :: i
        res%n = lhs%n
        do i = 1, size(v)
            res%n = res%n + v(i)%n
        end do
    end function total
end module traits_protocol_static_01_cell_m

module traits_protocol_static_01_generic_m
    use traits_protocol_static_01_contracts_m, only: IAccum
    implicit none
    private
    public :: grand
contains
    function grand{IAccum :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = x(1) * x + T(1)
    end function grand
end module traits_protocol_static_01_generic_m

program traits_protocol_static_01
    use traits_protocol_static_01_cell_m, only: Cell
    use traits_protocol_static_01_generic_m, only: grand
    implicit none
    type(Cell) :: c(3), r
    c = [Cell(1), Cell(2), Cell(3)]
    r = grand(c)
    print '(a,i0)', 'grand = ', r%n
    if (r%n /= 8) error stop 1
    print '(a)', 'traits_protocol_static_01 ok'
end program traits_protocol_static_01
