! A protocol with an initializer and a generic message requirement has no
! runtime dictionaries, but its conformance keeps working statically.
module traits_protocol_static_02_contracts_m
    implicit none
    private
    public :: IScaled, IKey

    abstract interface :: IKey
        function key(self) result(r)
            type(itself), intent(in), pass :: self
            integer :: r
        end function key
    end interface IKey

    abstract interface :: IScaled
        initial(n)
            integer, intent(in) :: n
        end initial
        function scaled{IKey :: U}(self, k) result(r)
            type(itself), intent(in), pass :: self
            type(U), intent(in) :: k
            integer :: r
        end function scaled
    end interface IScaled
end module traits_protocol_static_02_contracts_m

module traits_protocol_static_02_cell_m
    use traits_protocol_static_02_contracts_m, only: IScaled, IKey
    implicit none
    private
    type, public, sealed :: Cell
        integer :: n = 0
    contains
        procedure, pass :: scaled => cell_scaled
    end type Cell

    type, public, sealed, implements(IKey) :: Key
        integer :: k = 3
    contains
        procedure, pass :: key => key_of
    end type Key

    implements IScaled :: Cell
        initial :: make
    end implements Cell
contains
    function make(n) result(res)
        integer, intent(in) :: n
        type(Cell) :: res
        res%n = n
    end function make

    function key_of(self) result(r)
        class(Key), intent(in) :: self
        integer :: r
        r = self%k
    end function key_of

    function cell_scaled{IKey :: U}(self, k) result(r)
        class(Cell), intent(in) :: self
        type(U), intent(in) :: k
        integer :: r
        r = self%n * k%key()
    end function cell_scaled
end module traits_protocol_static_02_cell_m

program traits_protocol_static_02
    use traits_protocol_static_02_cell_m, only: Cell, Key
    implicit none
    type(Cell) :: c
    c = Cell(4)
    print '(a,i0)', 'scaled = ', c%scaled(Key())
    if (c%scaled(Key()) /= 12) error stop 1
    print '(a)', 'traits_protocol_static_02 ok'
end program traits_protocol_static_02
