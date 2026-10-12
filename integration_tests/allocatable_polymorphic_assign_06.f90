module allocatable_polymorphic_assign_06_m
    implicit none
    type :: base_t
        integer :: a = 0
    end type
    type, extends(base_t) :: child_t
        integer :: b = 0
        character(len=2) :: c(3) = ''
        character(len=2) :: e = ''
        integer, allocatable :: d(:)
    end type
    type, extends(base_t) :: ext_t
        character(len=2) :: c(3)
    end type
    type :: holder_t
        class(base_t), allocatable :: c(:)
    end type
contains
    function make() result(r)
        class(base_t), allocatable :: r(:)
        allocate(child_t :: r(3))
        select type (r)
        type is (child_t)
            r%a = [7, 8, 9]
            r%b = [4, 5, 6]
        end select
        call fill(r)
    end function

    ! Sets the character and allocatable components of the child_t
    ! elements of x from their a and b components.
    subroutine fill(x)
        class(base_t), intent(inout) :: x(:)
        integer :: i
        select type (x)
        type is (child_t)
            do i = 1, size(x)
                x(i)%c = ['ab', 'cd', 'ef']
                x(i)%e = 'e' // achar(iachar('0') + x(i)%a)
                x(i)%d = [x(i)%a, x(i)%b]
            end do
        end select
    end subroutine

    ! x has the dynamic type child_t, n elements, x(i)%a = a1 + i - 1,
    ! x(i)%b = b1 + (i - 1)*db and the components set by fill (d may be
    ! unallocated if d_may_be_lost is present).
    subroutine check_child(x, n, a1, b1, db, code, d_may_be_lost)
        class(base_t), intent(in) :: x(:)
        integer, intent(in) :: n, a1, b1, db, code
        logical, intent(in), optional :: d_may_be_lost
        integer :: i
        if (size(x) /= n) error stop code
        select type (x)
        type is (child_t)
            do i = 1, n
                if (x(i)%a /= a1 + i - 1) error stop code + 1
                if (x(i)%b /= b1 + (i - 1)*db) error stop code + 2
                if (any(x(i)%c /= ['ab', 'cd', 'ef'])) error stop code + 4
                if (x(i)%e /= 'e' // achar(iachar('0') + x(i)%a)) &
                    error stop code + 5
                if (.not. allocated(x(i)%d)) then
                    if (present(d_may_be_lost)) cycle
                    error stop code + 6
                end if
                if (size(x(i)%d) /= 2) error stop code + 7
                if (any(x(i)%d /= [x(i)%a, x(i)%b])) error stop code + 8
            end do
        class default
            error stop code + 3
        end select
    end subroutine
end module

program allocatable_polymorphic_assign_06
    ! F2018 10.2.1.3: an allocatable polymorphic array that is (re)allocated
    ! by an intrinsic assignment takes the dynamic type of a polymorphic
    ! expression, and is deallocated first if its shape or dynamic type
    ! differs from that of the expression.
    use allocatable_polymorphic_assign_06_m
    implicit none
    class(base_t), allocatable :: s(:), t(:), u(:), v(:), w(:)
    class(*), allocatable :: x(:)
    type(holder_t) :: h

    allocate(child_t :: s(3))
    select type (s)
    type is (child_t)
        s%a = [1, 2, 3]
        s%b = [10, 20, 30]
    end select
    call fill(s)

    t = s
    call check_child(t, 3, 1, 10, 10, 10)

    ! Same shape and dynamic type: t is not reallocated.
    t = s
    call check_child(t, 3, 1, 10, 10, 90)

    u = s(2:3)
    call check_child(u, 2, 2, 20, 10, 20)

    ! d must be copied here (F2018 10.2.1.3), but GFortran 13 has a bug
    ! that leaves it unallocated, so only its values are checked.
    t = t(2:3)
    call check_child(t, 2, 2, 20, 10, 30, d_may_be_lost=.true.)

    t = make()
    call check_child(t, 3, 7, 4, 1, 40)

    allocate(base_t :: v(3))
    v = s
    call check_child(v, 3, 1, 10, 10, 50)

    h%c = s(1:2)
    call check_child(h%c, 2, 1, 10, 10, 60)

    allocate(w, source=s)
    call check_child(w, 3, 1, 10, 10, 70)

    x = s
    select type (x)
    type is (child_t)
        if (size(x) /= 3) error stop 80
        if (any(x%b /= [10, 20, 30])) error stop 81
        if (x(3)%c(3) /= 'ef') error stop 83
        if (any(x(2)%d /= [2, 20])) error stop 84
    class default
        error stop 82
    end select

    call copy_ext()
    print *, "ok"

contains

    ! lfortran/lfortran#13598: `y = x` keeps the dynamic type ext_t of x,
    ! including a character array component.
    subroutine copy_ext()
        class(base_t), allocatable :: x(:), y(:)
        allocate(ext_t :: x(2))
        select type (x)
        type is (ext_t)
            x(2)%c = ['ab', 'cd', 'ef']
        end select
        y = x
        select type (y)
        type is (ext_t)
            if (size(y) /= 2) error stop 100
            if (y(2)%c(3) /= 'ef') error stop 101
        class default
            error stop 102
        end select
    end subroutine
end program
