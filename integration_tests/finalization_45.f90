! ALLOCATE with SOURCE= defines the new array elements as copies of the
! source: no finalization takes place (F2018 7.5.6.3 lists none for
! ALLOCATE), whether the element type is finalizable itself or only through
! a component, and no defined assignment of a component is invoked, as it
! would be by an intrinsic assignment.
module finalization_45_m
    implicit none
    integer :: nf = 0
    integer :: nfc = 0
    integer :: na = 0

    type :: item
        integer :: v = 0
    contains
        final :: fin
    end type

    type, extends(item) :: sub
        integer :: w = 0
    end type

    type :: comp_t
        integer :: k = 0
    contains
        final :: fin_comp
    end type

    type :: holder_t
        type(comp_t) :: d
        integer, allocatable :: x(:)
    end type

    type :: asg_t
        integer :: k = 0
    contains
        procedure :: asg
        generic :: assignment(=) => asg
    end type

    type :: asg_holder_t
        type(asg_t) :: c
    end type
contains
    subroutine fin(x)
        type(item), intent(inout) :: x
        nf = nf + 1
    end subroutine

    subroutine fin_comp(x)
        type(comp_t), intent(inout) :: x
        nfc = nfc + 1
    end subroutine

    subroutine asg(a, b)
        class(asg_t), intent(out) :: a
        type(asg_t), intent(in) :: b
        na = na + 1
        a%k = b%k
    end subroutine
end module

program finalization_45
    use finalization_45_m
    implicit none
    type(item) :: za(2), z
    type(item), allocatable :: ca(:), cb(:)
    class(item), allocatable :: cza(:), cca(:)
    type(holder_t) :: a
    type(holder_t), allocatable :: arr(:)
    type(asg_holder_t) :: hs(2)
    type(asg_holder_t), allocatable :: ha(:)

    ! Array source of a finalizable type.
    za%v = [1, 2]
    nf = 0
    allocate(ca, source=za)
    if (nf /= 0) error stop 1
    if (size(ca) /= 2) error stop 2
    if (any(ca%v /= [1, 2])) error stop 3

    ! Polymorphic array source: the new array has its dynamic type.
    allocate(sub :: cza(2))
    select type (cza)
    type is (sub)
        cza%v = [3, 4]
        cza%w = [5, 6]
    end select
    nf = 0
    allocate(cca, source=cza)
    if (nf /= 0) error stop 4
    if (size(cca) /= 2) error stop 5
    select type (cca)
    type is (sub)
        if (any(cca%v /= [3, 4])) error stop 6
        if (any(cca%w /= [5, 6])) error stop 7
    class default
        error stop 8
    end select

    ! Scalar source broadcast to every element.
    z%v = 7
    nf = 0
    allocate(cb(3), source=z)
    if (nf /= 0) error stop 9
    if (any(cb%v /= 7)) error stop 10

    ! A type that is finalizable only through a component, with an
    ! allocatable component that is copied deeply.
    a%d%k = 8
    allocate(a%x(3))
    a%x = [1, 2, 3]
    nfc = 0
    allocate(arr(2), source=a)
    if (nfc /= 0) error stop 11
    if (any(arr%d%k /= 8)) error stop 12
    if (any(arr(2)%x /= [1, 2, 3])) error stop 13
    arr(1)%x(1) = 99
    if (a%x(1) /= 1) error stop 14
    if (arr(2)%x(1) /= 1) error stop 15

    ! A component with a defined assignment is copied, not assigned.
    hs(1)%c%k = 4
    hs(2)%c%k = 5
    na = 0
    allocate(ha, source=hs)
    if (na /= 0) error stop 16
    if (any(ha%c%k /= [4, 5])) error stop 17

    print *, "ok"
end program
