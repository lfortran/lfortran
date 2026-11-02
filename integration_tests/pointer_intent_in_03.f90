! A pointer, intent(in) array dummy given a nonpointer target array: an
! optional actual that is absent stays absent, components of derived types
! are associated, and the dummy aliases the actual. The other actuals of a
! call forwarding an optional are evaluated once, also for type-bound and
! procedure pointer calls.
module pointer_intent_in_03_mod
    implicit none
    type :: holder
        real :: arr(2, 3)
        integer :: v(0:4)
    end type
    type :: counter
    contains
        procedure, nopass :: count_present
    end type
    interface
        subroutine count_present_iface(p, q, n)
            integer, pointer, optional, intent(in) :: p(:), q(:)
            integer, intent(out) :: n
        end subroutine
    end interface
    integer, pointer :: saved(:) => null()
    integer :: evaluations = 0
contains
    integer function next_size()
        evaluations = evaluations + 1
        next_size = evaluations
    end function

    subroutine sizes(p, k, q, total)
        integer, pointer, optional, intent(in) :: p(:)
        integer, intent(in) :: k
        integer, pointer, optional, intent(in) :: q(:)
        integer, intent(out) :: total
        total = k
        if (present(p)) total = total + 10*size(p)
        if (present(q)) total = total + 100*size(q)
    end subroutine

    subroutine forward_twice(x, total)
        integer, optional, target, intent(inout) :: x(:)
        integer, intent(out) :: total
        call sizes(x, next_size(), x, total)
    end subroutine

    subroutine forward_bound(x, y, n)
        integer, optional, target, intent(inout) :: x(:), y(:)
        integer, intent(out) :: n
        type(counter) :: c
        procedure(count_present_iface), pointer :: f
        integer :: m
        call c%count_present(q=y, n=n, p=x)
        f => count_present
        call f(x, y, m)
        if (m /= n) error stop "procedure pointer"
    end subroutine

    subroutine count_present(p, q, n)
        integer, pointer, optional, intent(in) :: p(:), q(:)
        integer, intent(out) :: n
        n = 0
        if (present(p)) then
            if (.not. associated(p)) error stop "p not associated"
            n = n + size(p)
        end if
        if (present(q)) then
            if (.not. associated(q)) error stop "q not associated"
            n = n + 10*size(q)
        end if
    end subroutine

    subroutine forward(x, y, n)
        integer, optional, target, intent(inout) :: x(:)
        integer, optional, target, intent(inout) :: y(:)
        integer, intent(out) :: n
        call count_present(x, y, n)
    end subroutine

    subroutine forward_2d(x, present_)
        real, optional, target, intent(inout) :: x(:, :)
        logical, intent(out) :: present_
        call is_present_2d(x, present_)
    end subroutine

    subroutine is_present_2d(p, present_)
        real, pointer, optional, intent(in) :: p(:, :)
        logical, intent(out) :: present_
        present_ = present(p)
        if (present_) p(1, 1) = -1.0
    end subroutine

    subroutine negate_first(p)
        integer, pointer, intent(in) :: p(:)
        p(lbound(p, 1)) = -p(lbound(p, 1))
    end subroutine

    subroutine shape_2d(p, rows, cols)
        real, pointer, intent(in) :: p(:, :)
        integer, intent(out) :: rows, cols
        rows = size(p, 1)
        cols = size(p, 2)
    end subroutine

    subroutine save_and_add(p)
        integer, pointer, intent(in) :: p(:)
        saved => p
        p(1) = p(1) + 100
    end subroutine
end module

program pointer_intent_in_03
    use pointer_intent_in_03_mod
    implicit none
    integer, target :: a(3), b(2), c(0:2)
    real, target :: r(2, 2)
    type(holder), target :: h, hs(2)
    integer :: n, rows, cols
    logical :: present_

    a = [1, 2, 3]
    b = [4, 5]
    call forward(n=n)
    if (n /= 0) error stop "both absent"
    call forward(a, n=n)
    if (n /= 3) error stop "x present, y absent"
    call forward(y=b, n=n)
    if (n /= 20) error stop "x absent, y present"
    call forward(a, b, n)
    if (n /= 23) error stop "both present"
    call forward_bound(y=b, n=n)
    if (n /= 20) error stop "type-bound, x absent"
    call forward_bound(a, b, n)
    if (n /= 23) error stop "type-bound, both present"

    call forward_twice(total=n)
    if (n /= 1 .or. evaluations /= 1) error stop "twice, absent"
    call forward_twice(b, n)
    if (n /= 222 .or. evaluations /= 2) error stop "twice, present"

    r = 2.0
    call forward_2d(present_=present_)
    if (present_) error stop "2d absent"
    call forward_2d(r, present_)
    if (.not. present_) error stop "2d present"
    if (r(1, 1) /= -1.0) error stop "2d not modified"

    h%arr = 3.0
    h%v = [1, 2, 3, 4, 5]
    hs(1)%v = 9
    hs(2)%v = 9
    call shape_2d(h%arr, rows, cols)
    if (rows /= 2 .or. cols /= 3) error stop "component shape"
    call negate_first(h%v)
    if (h%v(0) /= -1) error stop "component not modified"
    call negate_first(hs(2)%v)
    if (hs(2)%v(0) /= -9 .or. hs(1)%v(0) == -9) error stop "element component"

    call save_and_add(a)
    if (a(1) /= 101) error stop "write through dummy lost"
    if (.not. associated(saved, a)) error stop "saved not associated with a"
    c = [7, 8, 9]
    call save_and_add(c)
    if (.not. associated(saved, c)) error stop "saved not associated with c"
    if (lbound(saved, 1) /= 0 .or. c(1) /= 108) error stop "lower bound"
    saved(2) = 0
    if (c(2) /= 0) error stop "saved does not alias c"
    print *, "ok"
end program
