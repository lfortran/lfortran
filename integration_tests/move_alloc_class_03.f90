module move_alloc_class_03_m
    use iso_c_binding, only: c_int
    implicit none
    type :: base
        integer :: x = 1
    end type
    type, extends(base) :: child
        integer :: y = 2
    end type
    type, bind(c) :: bc
        integer(c_int) :: k
    end type
    type :: sq
        sequence
        integer :: k
    end type
    type(bc), allocatable :: gbc
    type(sq), allocatable :: gsq
contains
    ! Separate definitions of a BIND(C) or SEQUENCE type with the same name
    ! and components are the same type (F2018 7.5.2.4)
    subroutine move_same_type()
        type, bind(c) :: bc
            integer(c_int) :: k
        end type
        type :: sq
            sequence
            integer :: k
        end type
        type(bc), allocatable :: lbc
        type(sq), allocatable :: lsq
        allocate(gbc)
        gbc%k = 7
        call move_alloc(gbc, lbc)
        if (allocated(gbc) .or. .not. allocated(lbc)) error stop 26
        if (lbc%k /= 7) error stop 27
        allocate(gsq)
        gsq%k = 8
        call move_alloc(gsq, lsq)
        if (allocated(gsq) .or. .not. allocated(lsq)) error stop 28
        if (lsq%k /= 8) error stop 29
    end subroutine
end module

program move_alloc_class_03
    ! Type compatible `from`/`to` pairs accepted by move_alloc
    use move_alloc_class_03_m
    implicit none
    class(base), allocatable :: a, b
    type(base), allocatable :: c, l
    class(base), allocatable :: d, f, h
    type(child), allocatable :: e
    class(child), allocatable :: g
    integer, allocatable :: i(:), m(:), n, o
    class(*), allocatable :: j(:), p, q
    type(base), allocatable :: k
    real(8), allocatable :: r(:,:), s(:,:)

    allocate(child :: a)
    call move_alloc(a, b)
    if (allocated(a) .or. .not. allocated(b)) error stop 1
    select type (b)
    type is (child)
        if (b%y /= 2) error stop 2
    class default
        error stop 3
    end select

    allocate(c)
    c%x = 5
    call move_alloc(c, d)
    if (allocated(c)) error stop 4
    if (d%x /= 5) error stop 5

    allocate(e)
    e%y = 7
    call move_alloc(e, f)
    select type (f)
    type is (child)
        if (f%y /= 7) error stop 6
    class default
        error stop 7
    end select

    allocate(g)
    g%y = 8
    call move_alloc(g, h)
    select type (h)
    type is (child)
        if (h%y /= 8) error stop 8
    class default
        error stop 9
    end select

    allocate(i(3))
    i = 4
    call move_alloc(i, j)
    if (allocated(i)) error stop 10
    select type (j)
    type is (integer)
        if (size(j) /= 3) error stop 11
        if (any(j /= 4)) error stop 12
    class default
        error stop 13
    end select

    allocate(k)
    k%x = 9
    call move_alloc(k, l)
    if (allocated(k)) error stop 14
    if (l%x /= 9) error stop 15

    allocate(p, source=6)
    call move_alloc(p, q)
    if (allocated(p)) error stop 16
    select type (q)
    type is (integer)
        if (q /= 6) error stop 17
    class default
        error stop 18
    end select

    allocate(m(2))
    m = [1, 2]
    call move_alloc(m, i)
    if (allocated(m)) error stop 19
    if (any(i /= [1, 2])) error stop 20

    allocate(n)
    n = 3
    call move_alloc(n, o)
    if (allocated(n)) error stop 21
    if (o /= 3) error stop 22

    allocate(r(2, 3))
    r = 1.5d0
    call move_alloc(r, s)
    if (allocated(r)) error stop 23
    if (any(shape(s) /= [2, 3])) error stop 24
    if (any(s /= 1.5d0)) error stop 25

    call move_same_type()
    print *, "ok"
end program
