module allocatable_polymorphic_assign_04_m
    implicit none
    type :: child_t
        integer :: b = 0
    end type
    type :: other_t
        real :: c = 0
    end type
    type, extends(child_t) :: grandchild_t
        integer :: d = 0
    end type
    type :: holder_t
        class(*), allocatable :: u(:)
    end type
    integer :: ncalls = 0
contains
    function make_array(n) result(r)
        integer, intent(in) :: n
        integer, allocatable :: r(:)
        integer :: i
        ncalls = ncalls + 1
        allocate(r(n))
        r = [(10*i, i = 1, n)]
    end function

    function grow(x) result(r)
        class(*), intent(in) :: x(:)
        integer, allocatable :: r(:)
        ncalls = ncalls + 1
        allocate(r(size(x) + 1))
        select type (x)
        type is (integer)
            r(1:size(x)) = x
        class default
            r(1:size(x)) = -1
        end select
        r(size(r)) = 99
    end function
end module

program allocatable_polymorphic_assign_04
    ! F2003 allocate-on-assignment: an allocated class(*), allocatable
    ! array is reallocated when the shape or the dynamic type of the RHS
    ! differs, and takes the dynamic type and shape of the RHS.
    use allocatable_polymorphic_assign_04_m
    implicit none
    class(*), allocatable :: u(:), w(:,:)
    class(child_t), allocatable :: v(:)
    type(child_t) :: c(3)
    type(other_t) :: o(3)
    type(grandchild_t) :: g(3)
    type(holder_t) :: h
    integer :: z(0:3), a(5)

    ! Same type, different size
    u = [1, 2]
    u = [1, 2, 3]
    select type (u)
    type is (integer)
        if (size(u) /= 3) error stop 1
        if (any(u /= [1, 2, 3])) error stop 2
    class default
        error stop 3
    end select

    ! Different type, different size
    u = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
    select type (u)
    type is (real(8))
        if (size(u) /= 4) error stop 4
        if (any(u /= [1.0d0, 2.0d0, 3.0d0, 4.0d0])) error stop 5
    class default
        error stop 6
    end select

    ! Different type, same size
    u = [5, 6, 7, 8]
    select type (u)
    type is (integer)
        if (size(u) /= 4) error stop 7
        if (any(u /= [5, 6, 7, 8])) error stop 8
    class default
        error stop 9
    end select

    ! Derived type, then another derived type of the same size
    c%b = 7
    u = c
    select type (u)
    type is (child_t)
        if (size(u) /= 3) error stop 10
        if (any(u%b /= 7)) error stop 11
    class default
        error stop 12
    end select
    o%c = 2.5
    u = o
    select type (u)
    type is (other_t)
        if (size(u) /= 3) error stop 13
        if (any(u%c /= 2.5)) error stop 14
    class default
        error stop 15
    end select

    ! Derived type, then an intrinsic type of another size
    u = [1.5d0, 2.5d0, 3.5d0, 4.5d0]
    select type (u)
    type is (real(8))
        if (size(u) /= 4) error stop 16
        if (abs(u(4) - 4.5d0) > 1.0d-12) error stop 17
    class default
        error stop 18
    end select

    ! Same size, different shape
    w = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    w = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    if (size(w, 1) /= 3 .or. size(w, 2) /= 2) error stop 19
    select type (w)
    type is (integer)
        if (w(3, 2) /= 6) error stop 20
    class default
        error stop 21
    end select

    ! Same size, different character length
    u = ["ab", "cd"]
    u = ["xyz", "uvw"]
    select type (u)
    type is (character(*))
        if (len(u) /= 3) error stop 22
        if (u(2) /= "uvw") error stop 23
    class default
        error stop 24
    end select

    ! A function result is evaluated once, before the variable is
    ! deallocated, also when the variable is an argument of the function
    ncalls = 0
    u = make_array(3)
    if (ncalls /= 1) error stop 25
    u = make_array(3)
    if (ncalls /= 2) error stop 26
    u = make_array(4)
    if (ncalls /= 3) error stop 27
    select type (u)
    type is (integer)
        if (size(u) /= 4) error stop 28
        if (any(u /= [10, 20, 30, 40])) error stop 29
    class default
        error stop 30
    end select
    u = [1, 2]
    ncalls = 0
    u = grow(u)
    if (ncalls /= 1) error stop 31
    select type (u)
    type is (integer)
        if (size(u) /= 3) error stop 32
        if (any(u /= [1, 2, 99])) error stop 33
    class default
        error stop 34
    end select

    ! The bounds are those of the expression
    z = [4, 5, 6, 7]
    u = z
    if (lbound(u, 1) /= 0 .or. ubound(u, 1) /= 3) error stop 35

    ! class(t): same size, extended type
    c%b = 1
    v = c
    g%d = 5
    v = g
    select type (v)
    type is (grandchild_t)
        if (size(v) /= 3) error stop 36
        if (any(v%d /= 5)) error stop 37
    class default
        error stop 38
    end select

    ! Vector subscript: different type, then different size
    a = [1, 2, 3, 4, 5]
    u = [1.5, 2.5, 3.5]
    u = a([1, 3, 5])
    select type (u)
    type is (integer)
        if (size(u) /= 3) error stop 39
        if (any(u /= [1, 3, 5])) error stop 40
    class default
        error stop 41
    end select
    u = a([2, 4])
    select type (u)
    type is (integer)
        if (size(u) /= 2) error stop 42
        if (any(u /= [2, 4])) error stop 43
    class default
        error stop 44
    end select

    ! class(*) component: different type, then different size
    h%u = [1.5, 2.5]
    h%u = [1, 2]
    select type (q => h%u)
    type is (integer)
        if (size(q) /= 2) error stop 45
        if (any(q /= [1, 2])) error stop 46
    class default
        error stop 47
    end select
    h%u = [3, 4, 5]
    select type (q => h%u)
    type is (integer)
        if (size(q) /= 3) error stop 48
        if (any(q /= [3, 4, 5])) error stop 49
    class default
        error stop 50
    end select
    print *, "PASS"
end program allocatable_polymorphic_assign_04
