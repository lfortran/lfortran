module pdt_30_mod
    ! Components and LEN type parameters of a parameterized derived type as
    ! actual arguments of calls that are resolved in different ways: a
    ! passed object that is not the first dummy argument, generic and
    ! type-bound functions, procedure pointers and dummy procedures.
    implicit none

    type :: vec_t(n)
        integer, len :: n
        real :: x(n)
    contains
        procedure, pass(self) :: scale_into
        procedure, pass(self) :: add_len
        procedure :: total
    end type

    interface twice
        module procedure twice_len
    end interface

    abstract interface
        subroutine fill_i(a, k)
            real, intent(inout) :: a(:)
            integer, intent(in) :: k
        end subroutine
    end interface

contains

    subroutine scale_into(a, b, self)
        real, intent(inout) :: a(:)
        real, allocatable, intent(inout) :: b(:)
        class(vec_t(*)), intent(in) :: self
        a = 2 * a
        if (allocated(b)) deallocate(b)
        allocate(b(self%n + 1))
        b = 7
    end subroutine

    subroutine add_len(k, self)
        integer, intent(inout) :: k
        class(vec_t(*)), intent(in) :: self
        k = k + self%n
    end subroutine

    real function total(self, a)
        class(vec_t(*)), intent(in) :: self
        real, intent(inout) :: a(:)
        a = a + 1
        total = sum(a) + self%n
    end function

    integer function twice_len(k)
        integer, intent(in) :: k
        twice_len = 2 * k
    end function

    subroutine fill(a, k)
        real, intent(inout) :: a(:)
        integer, intent(in) :: k
        a = k
    end subroutine

    subroutine apply(f, w)
        procedure(fill_i) :: f
        type(vec_t(*)), intent(inout) :: w
        call f(w%x, w%n)
    end subroutine

end module

program pdt_30
    use pdt_30_mod
    implicit none
    type(vec_t(3)) :: v
    real, allocatable :: tmp(:)
    procedure(fill_i), pointer :: pf => null()
    integer :: k
    real :: r

    v%x = 1
    call v%scale_into(v%x, tmp)
    if (any(v%x /= 2)) error stop 1
    if (size(tmp) /= 4) error stop 2
    if (any(tmp /= 7)) error stop 3

    k = 10
    call v%add_len(k)
    if (k /= 13) error stop 4

    r = v%total(v%x)
    if (any(v%x /= 3)) error stop 5
    if (abs(r - 12) > 1e-6) error stop 6

    if (twice(v%n) /= 6) error stop 7

    pf => fill
    call pf(v%x, v%n)
    if (any(v%x /= 3)) error stop 8

    v%x = 0
    call apply(fill, v)
    if (any(v%x /= 3)) error stop 9

    if (v%n /= 3 .or. size(v%x) /= 3) error stop 10
    print *, "ok"
end program
