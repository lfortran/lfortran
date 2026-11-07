! same_type_as with polymorphic array arguments must use the dynamic type
! (it cannot be folded at compile time)
module same_type_as_01_mod
    implicit none
    type, abstract :: vector
    end type
    type, extends(vector) :: grid_vector
        real :: x = 1.0
    end type
    type :: t
    end type
    type, extends(t) :: e
    end type
end module

program same_type_as_01
    use same_type_as_01_mod
    implicit none
    type(grid_vector) :: g
    class(vector), allocatable :: s, a(:), b(:), c(:)
    class(t), allocatable :: p(:), q(:), r(:)
    type(t) :: y
    type(e) :: x
    type(t) :: ty(2)

    allocate(s, source=g)
    allocate(a(2), source=g)
    allocate(b(2), mold=g)
    allocate(grid_vector :: c(2))

    if (.not. same_type_as(s, g)) error stop 1
    if (.not. same_type_as(a(1), g)) error stop 2
    if (.not. same_type_as(b(1), g)) error stop 3
    if (.not. same_type_as(c(1), g)) error stop 4
    if (.not. same_type_as(a, g)) error stop 5
    if (.not. same_type_as(g, a)) error stop 6
    if (.not. same_type_as(b, g)) error stop 7
    if (.not. same_type_as(c, g)) error stop 8
    if (.not. same_type_as(a, b)) error stop 9

    select type (a)
    type is (grid_vector)
        if (size(a) /= 2) error stop 10
        if (any(abs(a%x - 1.0) > 1e-6)) error stop 11
    class default
        error stop 12
    end select

    allocate(e :: p(1))
    allocate(t :: q(1))
    allocate(r(1), source=x)

    ! class array vs type scalar, both argument orders
    if (.not. same_type_as(p, x)) error stop 20
    if (.not. same_type_as(x, p)) error stop 21
    if (same_type_as(p, y)) error stop 22
    if (same_type_as(y, p)) error stop 23
    if (.not. same_type_as(q, y)) error stop 24
    if (same_type_as(q, x)) error stop 25

    ! class array vs class array
    if (same_type_as(p, q)) error stop 30
    if (same_type_as(q, p)) error stop 31
    if (.not. same_type_as(p, r)) error stop 32

    ! non-polymorphic arrays have a compile-time known type
    if (.not. same_type_as(ty, y)) error stop 40
    if (same_type_as(ty, x)) error stop 41
    if (.not. extends_type_of(x, ty)) error stop 42
    if (extends_type_of(ty, x)) error stop 43

    select type (r)
    type is (e)
        if (size(r) /= 1) error stop 50
    class default
        error stop 51
    end select

    print *, "ok"
end program
