! allocate(source=) defines a new object as a copy of the source: neither the
! new object nor its components are finalized by the copy (F2018 7.5.6.3),
! while an intrinsic assignment finalizes the components of its variable.
module finalization_44_m
    implicit none
    integer :: nfin = 0

    type :: comp_t
        integer :: k = 0
    contains
        final :: fin_comp
    end type

    type :: holder_t
        type(comp_t) :: d
        type(comp_t), allocatable :: c
        type(comp_t), allocatable :: cs(:)
        type(comp_t) :: fixed(2)
        class(comp_t), allocatable :: pc
    end type

    type, extends(holder_t) :: ext_holder_t
        type(comp_t) :: e
    end type
contains
    subroutine fin_comp(x)
        type(comp_t), intent(inout) :: x
        nfin = nfin + 1
    end subroutine
end module

program finalization_44
    use finalization_44_m
    implicit none
    type(holder_t) :: a, d
    type(holder_t), allocatable :: b
    type(ext_holder_t) :: ea
    type(ext_holder_t), allocatable :: eb

    a%d%k = 1
    allocate(a%c, a%cs(3))
    a%c%k = 2
    a%cs%k = [3, 4, 5]
    a%fixed%k = [6, 7]
    allocate(a%pc)
    a%pc%k = 8

    nfin = 0
    allocate(b, source=a)
    if (nfin /= 0) error stop 1
    if (b%d%k /= 1 .or. b%c%k /= 2) error stop 2
    if (any(b%cs%k /= [3, 4, 5]) .or. any(b%fixed%k /= [6, 7])) error stop 3
    if (b%pc%k /= 8) error stop 9
    b%c%k = 20
    b%cs(1)%k = 30
    if (a%c%k /= 2 .or. a%cs(1)%k /= 3) error stop 4

    ea%holder_t = a
    ea%e%k = 9
    nfin = 0
    allocate(eb, source=ea)
    if (nfin /= 0) error stop 5
    if (eb%e%k /= 9 .or. eb%c%k /= 2 .or. eb%cs(3)%k /= 5) error stop 6

    ! An intrinsic assignment finalizes the components of its variable.
    allocate(d%c, d%cs(3))
    nfin = 0
    d = a
    if (nfin == 0) error stop 7
    if (d%c%k /= 2 .or. any(d%cs%k /= [3, 4, 5])) error stop 8
    print *, "ok"
end program
