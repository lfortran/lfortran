module recursion_06_mod
implicit none

abstract interface
    subroutine callback()
    end subroutine callback
end interface

procedure(callback), pointer :: saved_proc => null()
integer :: seen(3) = 0

type :: solver
    integer :: id = 0
contains
    procedure :: solve
end type solver

contains

! Another actual argument of the call reads an element of the host variable
recursive subroutine elem_arg(x, s, depth)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: s, depth
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (s /= x(1)) error stop
        x(2) = x(2) + 1
        if (depth > 1) then
            allocate(y(3))
            y = depth
            call elem_arg(y, y(1), depth - 1)
            if (.not. allocated(y)) error stop
            if (any(y /= [depth, depth + 1, depth])) error stop
            call elem_arg(y, size(y) + depth - 3, depth - 1)
            if (any(y /= [depth, depth + 2, depth])) error stop
        end if
    end subroutine inner
end subroutine elem_arg

! Another actual argument calls a contained function that reads the host variable
recursive subroutine sibling_arg(x, s, depth)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: s, depth
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (s /= sum(x)) error stop
        x(1) = x(1) + 1
        if (depth > 1) then
            allocate(y(2))
            y = depth
            call sibling_arg(y, sum_y(), depth - 1)
            if (.not. allocated(y)) error stop
            if (any(y /= [depth + 1, depth])) error stop
        end if
    end subroutine inner

    integer function sum_y()
        sum_y = sum(y)
    end function sum_y
end subroutine sibling_arg

! A host function gets a variable of its host from the internal procedure
recursive integer function sum_host(x, depth) result(r)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, allocatable :: y(:)
    integer :: s
    r = 0
    call inner()
contains
    subroutine inner()
        if (.not. allocated(x)) error stop
        r = sum(x)
        if (depth > 1) then
            allocate(y(2))
            y = [depth, depth]
            y(1) = sum_host(y, depth - 1)
            if (.not. allocated(y)) error stop
            if (any(y /= [2*depth, depth])) error stop
            s = y(2) + sum_host(y, depth - 1)
            if (s /= 4*depth) error stop
            if (sum_host(y, depth - 1) /= 3*depth) then
                error stop
            else
                if (any(y /= [2*depth, depth])) error stop
            end if
            if (.not. allocated(y)) error stop
        end if
    end subroutine inner
end function sum_host

! A host function changes a scalar variable of its host passed from the internal procedure
recursive integer function count_up(k, depth) result(r)
    integer, intent(inout) :: k
    integer, intent(in) :: depth
    integer :: m, m2
    r = 0
    call inner()
contains
    subroutine inner()
        k = k + 1
        r = k
        if (depth > 1) then
            m = 10*depth
            m2 = count_up(m, depth - 1)
            if (m /= 10*depth + 1) error stop
            if (m2 /= 10*depth + 1) error stop
            if (k /= r) error stop
        end if
    end subroutine inner
end function count_up

! The host is called as a type-bound procedure
recursive subroutine solve(self, x, depth)
    class(solver), intent(in) :: self
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (.not. allocated(x)) error stop
        if (x(1) /= depth + self%id) error stop
        x = x + 1
        if (depth > 1) then
            allocate(y(2))
            y = depth - 1 + self%id
            call self%solve(y, depth - 1)
            if (.not. allocated(y)) error stop
            if (any(y /= depth + self%id)) error stop
        end if
    end subroutine inner
end subroutine solve

subroutine call_saved_proc()
    call saved_proc()
end subroutine call_saved_proc

! A contained procedure that calls nothing is called through a pointer after the host entered itself again
recursive subroutine pointer_after_recursion(depth)
    integer, intent(in) :: depth
    integer :: mine
    mine = depth
    call inner()
    if (depth > 1) call pointer_after_recursion(depth - 1)
    saved_proc => inner
    call call_saved_proc()
contains
    subroutine inner()
        seen(mine) = seen(mine) + 1
    end subroutine inner
end subroutine pointer_after_recursion

end module recursion_06_mod

