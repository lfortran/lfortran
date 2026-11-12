module array_constructor_21_mod
    implicit none
    type :: inner
        integer :: k
    end type
    type :: cell
        integer :: val
        character(len=2) :: tag
        type(inner) :: nest
    end type
    type :: list
        type(cell), allocatable :: items(:)
        integer :: n = 0
    contains
        procedure :: count => list_count
        procedure :: keys => list_keys
    end type
    type :: box
        type(cell), allocatable :: w(:)
    end type
    integer :: calls = 0
contains
    pure integer function n_used(v)
        integer, intent(in) :: v(:)
        n_used = count(v > 0)
    end function

    integer function nxt(k)
        integer, intent(in) :: k
        calls = calls + 1
        nxt = k
    end function

    pure function perm(n) result(r)
        integer, intent(in) :: n
        integer :: r(n)
        integer :: i
        do i = 1, n
            r(i) = n - i + 1
        end do
    end function

    pure integer function list_count(self)
        class(list), intent(in) :: self
        list_count = self%n
    end function

    function list_keys(self) result(r)
        class(list), intent(in) :: self
        integer, allocatable :: r(:)
        allocate(r(self%n))
        r = [self%items(1:self%count())%val]
    end function

    subroutine takes(v, s)
        integer, intent(in) :: v(:), s
        if (sum(v) /= s) error stop 60
    end subroutine
end module

program array_constructor_21
    ! A component of a section of a derived-type array in an array
    ! constructor, [a(lo:hi)%x], also when a subscript of the section calls a
    ! function. A section starts at 1 whatever the bounds of `a` are.
    use array_constructor_21_mod
    implicit none
    type(cell) :: f(5)
    type(cell), allocatable :: a(:), a0(:), g(:, :)
    type(cell), pointer :: p(:), p0(:)
    class(cell), allocatable :: c(:)
    type(list) :: l
    type(box), allocatable :: bs(:)
    integer :: v(4), i, j, n
    integer :: r2(2), r3(3), r4(4), r6(6), r7(7), r8(8)
    integer, allocatable :: keys(:)
    character(len=2) :: t2(2)
    real :: x3(3)

    allocate(a(5), a0(0:4), p(5), p0(0:4))
    do i = 1, 5
        a(i) = cell(9 + i, achar(96 + i) // "!", inner(100*i))
        a0(i - 1) = cell(19 + i, achar(96 + i) // "?", inner(200*i))
        p(i) = cell(29 + i, achar(64 + i) // "!", inner(300*i))
        p0(i - 1) = cell(39 + i, achar(64 + i) // "?", inner(400*i))
        f(i) = cell(49 + i, achar(96 + i) // "#", inner(500*i))
    end do
    allocate(c(5), source=a)
    allocate(l%items(0:9))
    l%items(0:4) = a
    l%n = 3
    allocate(bs(0:1))
    allocate(bs(0)%w(3), bs(1)%w(5:7))
    bs(0)%w = a(3:5)
    bs(1)%w = a(1:3)
    allocate(g(0:1, 5:7))
    n = 0
    do j = 5, 7
        do i = 0, 1
            n = n + 1
            g(i, j) = cell(n, "g ", inner(-n))
        end do
    end do
    v = [1, 0, 5, 7]

    ! Sections with constant bounds: fixed-size, allocatable and pointer
    ! bases, with a lower bound of 1 and of 0
    r2 = [a(2:3)%val]
    if (any(r2 /= [11, 12])) error stop 1
    r3 = [a0(0:2)%val]
    if (any(r3 /= [20, 21, 22])) error stop 2
    r3 = [p(3:)%val]
    if (any(r3 /= [32, 33, 34])) error stop 3
    r2 = [p0(3:4)%val]
    if (any(r2 /= [43, 44])) error stop 4
    r3 = [f(5:1:-2)%val]
    if (any(r3 /= [54, 52, 50])) error stop 5

    ! A bound of the section calls a pure function
    r3 = [a(1:n_used(v))%val]
    if (any(r3 /= [10, 11, 12])) error stop 6
    r4 = [a0(0:n_used(v))%val]
    if (any(r4 /= [20, 21, 22, 23])) error stop 7
    r3 = [p(:n_used(v))%val]
    if (any(r3 /= [30, 31, 32])) error stop 8
    r4 = [p0(0:n_used(v))%val]
    if (any(r4 /= [40, 41, 42, 43])) error stop 9
    r3 = [f(n_used(v):)%val]
    if (any(r3 /= [52, 53, 54])) error stop 10
    r3 = [a(5:1:-n_used(v(1:2)) - 1)%val]
    if (any(r3 /= [14, 12, 10])) error stop 11
    r3 = [c(1:n_used(v))%val]
    if (any(r3 /= [10, 11, 12])) error stop 12

    ! Other components, a constructor with a type, more than one value, and
    ! the constructor inside an expression and as an actual argument
    r3 = [a0(1:n_used(v))%nest%k]
    if (any(r3 /= [400, 600, 800])) error stop 13
    t2 = [p0(2:n_used(v))%tag]
    if (any(t2 /= ["C?", "D?"])) error stop 14
    x3 = [real :: a(1:n_used(v))%val]
    if (any(abs(x3 - [10.0, 11.0, 12.0]) > 1e-6)) error stop 15
    r7 = [0, a(1:n_used(v))%val, a0(n_used(v) - 1:)%val]
    if (any(r7 /= [0, 10, 11, 12, 22, 23, 24])) error stop 16
    if (sum([p0(0:n_used(v) - 1)%val]) /= 123) error stop 17
    call takes([a(n_used(v):n_used(v) + 1)%val], 25)

    ! A bound of the section calls a function that is not pure
    r3 = [a(nxt(1):nxt(3))%val]
    if (any(r3 /= [10, 11, 12])) error stop 18
    r3 = [a0(nxt(1):nxt(3))%val]
    if (any(r3 /= [21, 22, 23])) error stop 19
    r3 = [p0(nxt(0):nxt(2))%val]
    if (any(r3 /= [40, 41, 42])) error stop 20
    r3 = [f(nxt(2):nxt(4))%val]
    if (any(r3 /= [51, 52, 53])) error stop 21

    ! A bound of the section calls a type-bound function
    r4 = [l%items(0:l%count())%val]
    if (any(r4 /= [10, 11, 12, 13])) error stop 23
    keys = l%keys()
    if (size(keys) /= 3) error stop 24
    if (any(keys /= [11, 12, 13])) error stop 25
    r4 = [l%items(0:n_used(v))%nest%k]
    if (any(r4 /= [100, 200, 300, 400])) error stop 26

    ! A vector subscript that is a function result
    r3 = [a(perm(3))%val]
    if (any(r3 /= [12, 11, 10])) error stop 27
    r3 = [a0(perm(3))%val]
    if (any(r3 /= [23, 22, 21])) error stop 28
    r3 = [p0(perm(3) - 1)%val]
    if (any(r3 /= [42, 41, 40])) error stop 29

    ! The array is a component of an element of another array: a section of
    ! it, the whole of it with a lower bound of 5, and the whole of it when
    ! a function selects the element
    r2 = [bs(1)%w(nxt(6):)%val]
    if (any(r2 /= [11, 12])) error stop 30
    n = 1
    r3 = [bs(n)%w%val]
    if (any(r3 /= [10, 11, 12])) error stop 31
    r3 = [bs(n - 1)%w%nest%k]
    if (any(r3 /= [300, 400, 500])) error stop 32
    r3 = [bs(nxt(0))%w%val]
    if (any(r3 /= [12, 13, 14])) error stop 40
    r3 = [bs(n_used(v) - 3)%w%nest%k]
    if (any(r3 /= [300, 400, 500])) error stop 41

    ! A section as the value of an implied do, of rank 1 and of rank 2
    r6 = [(a0(1:n_used(v))%val, i = 1, 2)]
    if (any(r6 /= [21, 22, 23, 21, 22, 23])) error stop 33
    r8 = [(g(:, 5:6)%val, i = 1, 2)]
    if (any(r8 /= [1, 2, 3, 4, 1, 2, 3, 4])) error stop 34
    r8 = [(g(:, n_used(v) + 3:)%val, i = 1, 2)]
    if (any(r8 /= [3, 4, 5, 6, 3, 4, 5, 6])) error stop 35
    r8 = [(g(:, nxt(6):nxt(7))%val, i = 1, 2)]
    if (any(r8 /= [3, 4, 5, 6, 3, 4, 5, 6])) error stop 36
    r8 = [(g(:, i:i + 1)%nest%k, i = 5, 6)]
    if (any(r8 /= [-1, -2, -3, -4, -3, -4, -5, -6])) error stop 37

    ! A dummy argument with a lower bound of 0
    call check_assumed_shape(a)

    deallocate(p, p0)
    print *, "OK"

contains

    subroutine check_assumed_shape(x)
        type(cell), intent(in) :: x(0:)
        integer :: r(3)
        r = [x(0:n_used(v) - 1)%val]
        if (any(r /= [10, 11, 12])) error stop 38
        r = [x(nxt(1):nxt(3))%val]
        if (any(r /= [11, 12, 13])) error stop 39
    end subroutine

end program
