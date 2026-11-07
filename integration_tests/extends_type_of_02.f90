! extends_type_of with polymorphic array arguments must use the dynamic type
module extends_type_of_02_mod
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

program extends_type_of_02
    use extends_type_of_02_mod
    implicit none
    type(grid_vector) :: g
    class(vector), allocatable :: a(:), b(:), c(:)
    class(t), allocatable :: p(:), q(:), r(:)
    type(t) :: y
    type(e) :: x

    allocate(a(2), source=g)
    allocate(b(2), mold=g)
    allocate(grid_vector :: c(2))

    if (.not. extends_type_of(a, g)) error stop 1
    if (.not. extends_type_of(g, a)) error stop 2
    if (.not. extends_type_of(b, g)) error stop 3
    if (.not. extends_type_of(c, g)) error stop 4
    if (.not. extends_type_of(a, c)) error stop 5

    allocate(e :: p(1))
    allocate(t :: q(1))
    allocate(r(1), source=x)

    ! class array vs type scalar, both argument orders
    if (.not. extends_type_of(p, x)) error stop 10
    if (.not. extends_type_of(x, p)) error stop 11
    if (.not. extends_type_of(p, y)) error stop 12
    if (extends_type_of(y, p)) error stop 13
    if (extends_type_of(q, x)) error stop 14
    if (.not. extends_type_of(x, q)) error stop 15

    ! class array vs class array
    if (.not. extends_type_of(p, q)) error stop 20
    if (extends_type_of(q, p)) error stop 21
    if (.not. extends_type_of(r, p)) error stop 22

    print *, "ok"
end program
