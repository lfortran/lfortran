! A private module that owns instances of generic derived types and their
! runtime evidence; only its public procedures are its interface.
module traits_generic_type_04_owner
    use traits_generic_type_04_contracts, only: IAverager
    use traits_generic_type_04_simple, only: SimpleSum
    use traits_generic_type_04_pairwise, only: PairwiseSum
    use traits_generic_type_04_averager, only: Averager
    implicit none
    private
    public :: make_pair, pair_total, make_view, make_avg
contains
    function make_pair() result(p)
        type(PairwiseSum{SimpleSum}) :: p
        p = PairwiseSum(SimpleSum())
    end function make_pair
    integer function pair_total(p, x) result(r)
        type(PairwiseSum{SimpleSum}), intent(in) :: p
        integer, intent(in) :: x(:)
        r = p%sum(x)
    end function pair_total
    function make_view(key) result(v)
        integer, intent(in) :: key
        class(IAverager), allocatable :: v
        select case (key)
        case (1)
            v = Averager(drv = SimpleSum())
        case default
            v = Averager(drv = PairwiseSum(other = SimpleSum()))
        end select
    end function make_view
    function make_avg() result(a)
        type(Averager{SimpleSum}) :: a
        a = Averager(drv = SimpleSum())
    end function make_avg
end module traits_generic_type_04_owner
