! Generic derived type instances and their procedure copies have names no
! Fortran source can spell, and a constructor call lowers the instance it
! resolved rather than a name looked up again: user entities named like an
! instance, also with the leading underscores LFortran accepts, never
! collide with it or its copies or capture its constructor. An inferred
! structure constructor converts the values of components that are not of a
! parameter type like any structure constructor, and a procedure that uses a
! parameter only in a BLOCK belongs to its type.
module traits_generic_type_03_contracts
    implicit none
    private
    public :: INumeric, ISum
    abstract interface :: INumeric
        integer | real(8)
    end interface INumeric
    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum
end module traits_generic_type_03_contracts

module traits_generic_type_03_simple
    use traits_generic_type_03_contracts, only: ISum, INumeric
    implicit none
    private
    public :: SimpleSum
    type, sealed, implements(ISum) :: SimpleSum
    contains
        procedure, pass :: sum => simple_sum
    end type SimpleSum
contains
    function simple_sum{INumeric :: T}(self, x) result(s)
        type(SimpleSum), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum
end module traits_generic_type_03_simple

module traits_generic_type_03_types
    use traits_generic_type_03_contracts, only: ISum, INumeric
    implicit none
    private
    public :: PairwiseSum, Holder, Box, Tagged
    type, sealed, implements(ISum) :: PairwiseSum{ISum :: U}
        type(U), allocatable :: other
    contains
        initial :: init
        procedure, pass :: sum
    end type PairwiseSum
    type, sealed :: Holder{ISum :: U}
        type(U) :: item
        integer :: scale = 1
    contains
        procedure, pass :: total
    end type Holder
    type, sealed :: Box{ISum :: U}
        type(U) :: item
        integer :: n = 2
    contains
        procedure, pass :: twice
    end type Box
    type, sealed :: Tagged{ISum :: U}
        type(U), allocatable :: item
    contains
        procedure, nopass :: describe
    end type Tagged
contains
    function init(other) result(res)
        type(U), intent(in) :: other
        type(PairwiseSum{U}) :: res
        res%other = other
    end function init
    function sum{INumeric :: T}(self, x) result(s)
        type(PairwiseSum{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%other%sum(x)
    end function sum
    function total{INumeric :: T}(self, x) result(s)
        type(Holder{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(self%scale) * self%item%sum(x)
    end function total
    integer function twice(self) result(r)
        type(Box{U}), intent(in) :: self
        r = 2 * self%n
    end function twice
    ! Uses the type's parameter only in a BLOCK.
    integer function describe() result(r)
        r = 0
        block
            type(U), allocatable :: probe
            if (.not. allocated(probe)) r = 1
        end block
    end function describe
end module traits_generic_type_03_types

program traits_generic_type_03
    use traits_generic_type_03_simple, only: SimpleSum
    use traits_generic_type_03_types, only: PairwiseSum, Holder, Box, Tagged
    implicit none
    ! Declared after the instance, with the name an instance once had.
    type(PairwiseSum{SimpleSum}) :: p
    integer :: pairwisesum__simplesum
    integer :: __pairwisesum__simplesum, _scale
    type(Holder{SimpleSum}) :: h
    type(Tagged{SimpleSum}) :: t
    integer(8) :: big
    integer :: checks
    checks = 0
    p = PairwiseSum(SimpleSum())
    pairwisesum__simplesum = 5
    call check(p%sum([1, 2, 3]) + pairwisesum__simplesum == 11)
    ! The names an earlier compiler gave the instance and its initializer
    ! copy are the user's own too.
    __pairwisesum__simplesum = 7
    _scale = 2
    call check(_scale * p%sum([1, 2, 3]) + __pairwisesum__simplesum == 19)
    call check(__pairwisesum__simplesum_init(3) == 30)
    h = Holder(SimpleSum(), 2)
    call check(h%total([1, 2, 3]) == 12)
    call inner_local()
    call inner_dummy(make)
    ! Inferred structure constructors convert other components' values.
    b1 := Box(item = SimpleSum(), n = 2.5d0)
    call check(b1%twice() == 4)
    big = 4_8
    b2 := Box(item = SimpleSum(), n = big)
    call check(b2%twice() == 8)
    b3 := Box{SimpleSum}(item = SimpleSum(), n = 3.75d0)
    call check(b3%twice() == 6)
    call check(t%describe() == 1)
    print '(a, i0)', "traits_generic_type_03 checks: ", checks
contains
    subroutine check(condition)
        logical, intent(in) :: condition
        if (.not. condition) error stop
        checks = checks + 1
    end subroutine check
    integer function __pairwisesum__simplesum_init(n) result(r)
        integer, intent(in) :: n
        r = 10 * n
    end function __pairwisesum__simplesum_init
    function make(item, n) result(r)
        type(SimpleSum), intent(in) :: item
        integer, intent(in) :: n
        type(Holder{SimpleSum}) :: r
        r%item = item
        r%scale = 100 * n
    end function make
    ! A local entity named like an instance does not capture its constructor.
    subroutine inner_local()
        integer :: holder__simplesum
        type(Holder{SimpleSum}) :: g
        holder__simplesum = 4
        g = Holder(SimpleSum(), 3)
        call check(g%total([1, 2, 3]) + holder__simplesum == 22)
    end subroutine inner_local
    ! Nor does a dummy procedure with that name.
    subroutine inner_dummy(holder__simplesum)
        interface
            function holder__simplesum(item, n) result(r)
                import
                type(SimpleSum), intent(in) :: item
                integer, intent(in) :: n
                type(Holder{SimpleSum}) :: r
            end function holder__simplesum
        end interface
        type(Holder{SimpleSum}) :: g
        g = Holder(SimpleSum(), 3)
        call check(g%total([1, 2, 3]) == 18)
        g = holder__simplesum(SimpleSum(), 3)
        call check(g%total([1, 2, 3]) == 1800)
    end subroutine inner_dummy
end program traits_generic_type_03
