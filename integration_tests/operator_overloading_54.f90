! The size of a function result declared by size() of a dummy whose actual
! argument is a defined operation, here an elemental user minus of arrays
! whose extents are components of a derived-type dummy.
module operator_overloading_54_ops
    implicit none
    type :: iv
        real(8) :: lo = 0, hi = 0
    end type
    interface operator(-)
        module procedure sub
    end interface
contains
    elemental function sub(a, b) result(c)
        type(iv), intent(in) :: a, b
        type(iv) :: c
        c%lo = a%lo - b%hi
        c%hi = a%hi - b%lo
    end function
    pure function matvec(a, x) result(y)
        type(iv), intent(in) :: a(:, :), x(:)
        type(iv) :: y(size(a, 1))
        integer :: i
        do i = 1, size(a, 1)
            y(i)%lo = sum(a(i, :)%lo*x%lo)
            y(i)%hi = sum(a(i, :)%hi*x%hi)
        end do
    end function
end module

module operator_overloading_54_state
    use operator_overloading_54_ops, only: iv, operator(-), matvec
    implicit none
    type :: state_t
        integer :: n = 0
        type(iv), allocatable :: d(:)
    end type
contains
    subroutine step(state)
        type(state_t), intent(inout) :: state
        type(iv) :: qa(state%n, state%n), qb(state%n, state%n)
        qa%lo = 2
        qa%hi = 3
        qb%lo = 1
        qb%hi = 1
        state%d = matvec(qa - qb, state%d)
    end subroutine
end module

program operator_overloading_54
    use operator_overloading_54_ops, only: iv
    use operator_overloading_54_state, only: state_t, step
    implicit none
    type(state_t) :: s
    s%n = 2
    allocate(s%d(2))
    s%d%lo = 1
    s%d%hi = 1
    call step(s)
    print *, s%d%lo, s%d%hi
    if (any(s%d%lo /= 2) .or. any(s%d%hi /= 4)) error stop
end program
