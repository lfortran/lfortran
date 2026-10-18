! A VALUE dummy of derived type is a copy of the actual argument, including
! its allocatable, deferred-length and nested allocatable components, and
! pointer components with their association (F2018 15.5.2.3). Changing,
! deallocating or reallocating them in the callee does not affect the actual.
! Not labeled gfortran: GFortran copies VALUE dummies shallowly.
module value_attribute_07_m
    implicit none
    integer :: nfin = 0
    type :: fin_t
        integer :: k = 0
    contains
        final :: finalize_fin_t
    end type
    type :: fin_holder_t
        type(fin_t) :: d
        type(fin_t), allocatable :: c
        type(fin_t), allocatable :: cs(:)
        class(fin_t), allocatable :: pc
    end type
    type :: base_t
        integer :: k = 0
    end type
    type, extends(base_t) :: ext_t
        integer :: e = 0
    end type
    type :: inner_t
        integer, allocatable :: q(:)
        character(:), allocatable :: name
    end type
    type :: node_t
        integer :: v = 0
        type(node_t), allocatable :: next
    end type
    type :: item_t
        integer :: v = 0
        integer, allocatable :: a(:)
        real, allocatable :: r
        character(len=4) :: tag = "none"
        character(:), allocatable :: s
        type(inner_t) :: in
        type(inner_t), allocatable :: ins(:)
        integer, pointer :: p(:) => null()
        class(base_t), pointer :: cp => null()
        type(node_t) :: list
        class(base_t), allocatable :: poly
        class(*), allocatable :: any_int
        class(*), allocatable :: any_ext
    end type
contains
    subroutine finalize_fin_t(x)
        type(fin_t), intent(inout) :: x
        nfin = nfin + 1
    end subroutine

    ! The dummy and the copies of its components are not finalized.
    subroutine fin_value(x)
        type(fin_holder_t), value :: x
        if (nfin /= 0) error stop 42
        if (x%c%k /= 2 .or. any(x%cs%k /= [3, 4]) .or. x%pc%k /= 5) error stop 43
        x%cs(1)%k = 30
    end subroutine

    subroutine modify(x)
        type(item_t), value :: x
        if (x%v /= 8) error stop 1
        if (any(x%a /= [1, 2, 3])) error stop 2
        if (x%r /= 2.5) error stop 3
        if (x%tag /= "abcd") error stop 4
        if (x%s /= "abc") error stop 5
        if (any(x%in%q /= [7, 8]) .or. x%in%name /= "in") error stop 6
        if (size(x%ins) /= 2 .or. x%ins(2)%name /= "two") error stop 7
        if (.not. associated(x%p) .or. .not. associated(x%cp)) error stop 8
        if (x%list%next%next%v /= 3) error stop 9
        select type (q => x%poly)
        type is (ext_t)
            if (q%e /= 6) error stop 30
            q%e = 60
        class default
            error stop 31
        end select
        select type (q => x%any_int)
        type is (integer)
            if (q /= 7) error stop 32
            q = 70
        class default
            error stop 33
        end select
        select type (q => x%any_ext)
        type is (ext_t)
            if (q%k /= 1) error stop 34
            q%k = 10
        class default
            error stop 35
        end select
        x%v = 100
        x%a(2) = 99
        x%r = -1.0
        x%tag = "WXYZ"
        x%s(1:1) = "Z"
        x%in%q(1) = -1
        x%in%name = "changed"
        x%ins(2)%name = "TWO!"
        x%p(1) = 40
        x%list%next%next%v = 30
    end subroutine

    subroutine reallocate(x)
        type(item_t), value :: x
        deallocate(x%a)
        allocate(x%a(5))
        x%a = 0
        deallocate(x%r)
        x%s = "longer"
        deallocate(x%in%q)
        deallocate(x%ins)
        allocate(x%ins(1))
        x%ins(1)%name = "x"
        nullify(x%p)
        nullify(x%cp)
        deallocate(x%list%next)
        deallocate(x%poly, x%any_int, x%any_ext)
    end subroutine

    integer function sum_value(x) result(r)
        type(item_t), value :: x
        x%a = x%a + 1
        r = sum(x%a)
    end function

    subroutine unallocated(x)
        type(item_t), value :: x
        if (allocated(x%a) .or. allocated(x%s) .or. allocated(x%ins)) error stop 20
        allocate(x%a(4))
        x%s = "new"
    end subroutine
end module

program value_attribute_07
    use value_attribute_07_m
    implicit none
    integer, target :: b(2) = [4, 6]
    type(ext_t), target :: e
    type(item_t) :: y, z
    type(fin_holder_t) :: fh
    integer :: i

    y%v = 8
    allocate(y%a(3), y%in%q(2), y%ins(2), y%ins(1)%q(1), y%ins(2)%q(2))
    y%a = [1, 2, 3]
    allocate(y%r)
    y%r = 2.5
    y%tag = "abcd"
    y%s = "abc"
    y%in%q = [7, 8]
    y%in%name = "in"
    y%ins(1)%q = [1]
    y%ins(1)%name = "one"
    y%ins(2)%q = [2, 3]
    y%ins(2)%name = "two"
    y%p => b
    e%k = 3
    e%e = 5
    y%cp => e
    y%list%v = 1
    allocate(y%list%next)
    y%list%next%v = 2
    allocate(y%list%next%next)
    y%list%next%next%v = 3
    allocate(y%poly, source=ext_t(4, 6))
    allocate(y%any_int, source=7)
    allocate(y%any_ext, source=ext_t(1, 2))

    do i = 1, 3
        call modify(y)
        call reallocate(y)
    end do
    if (y%v /= 8) error stop 10
    if (size(y%a) /= 3 .or. any(y%a /= [1, 2, 3])) error stop 11
    if (.not. allocated(y%r)) error stop 12
    if (y%r /= 2.5) error stop 13
    if (y%tag /= "abcd") error stop 14
    if (y%s /= "abc") error stop 15
    if (.not. allocated(y%in%q)) error stop 16
    if (any(y%in%q /= [7, 8]) .or. y%in%name /= "in") error stop 17
    if (size(y%ins) /= 2 .or. y%ins(2)%name /= "two") error stop 18
    if (any(y%ins(2)%q /= [2, 3])) error stop 19
    if (.not. associated(y%p, b) .or. b(1) /= 40) error stop 21
    if (.not. associated(y%cp, e)) error stop 22
    select type (cp => y%cp)
    type is (ext_t)
        if (cp%e /= 5) error stop 23
    class default
        error stop 24
    end select
    if (.not. allocated(y%list%next)) error stop 28
    if (y%list%next%next%v /= 3) error stop 29
    select type (q => y%poly)
    type is (ext_t)
        if (q%k /= 4 .or. q%e /= 6) error stop 36
    class default
        error stop 37
    end select
    select type (q => y%any_int)
    type is (integer)
        if (q /= 7) error stop 38
    class default
        error stop 39
    end select
    select type (q => y%any_ext)
    type is (ext_t)
        if (q%k /= 1 .or. q%e /= 2) error stop 40
    class default
        error stop 41
    end select

    if (sum_value(y) /= 9) error stop 25
    if (any(y%a /= [1, 2, 3])) error stop 26

    call unallocated(z)
    if (allocated(z%a) .or. allocated(z%s)) error stop 27

    allocate(fh%c, fh%cs(2), fh%pc)
    fh%c%k = 2
    fh%cs%k = [3, 4]
    fh%pc%k = 5
    nfin = 0
    call fin_value(fh)
    if (nfin /= 0) error stop 44
    if (fh%cs(1)%k /= 3) error stop 45
    print *, y%a, y%s, y%in%q
end program
