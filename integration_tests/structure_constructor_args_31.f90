! An omitted nonallocatable component whose derived type has default
! initialization takes the default-initialized value of that type, as
! GFortran and Intel Fortran allow (#14488)
module structure_constructor_args_31_m
    implicit none
    type :: blah
        integer :: i = 1
    end type blah

    type :: blah2
        type(blah) :: b
        integer :: j = 2
    end type blah2
end module structure_constructor_args_31_m

program structure_constructor_args_31
    use structure_constructor_args_31_m, only: blah2
    implicit none

    type :: inner
        integer :: k
        integer :: i = 3
    end type inner

    type :: mid
        type(inner) :: n
    end type mid

    type :: parts
        integer, pointer :: p => null()
        integer, allocatable :: a(:)
        character(len=3) :: s = "abc"
        real :: v(3) = [1.0, 2.0, 3.0]
    end type parts

    type :: with_parts
        type(parts) :: q
        integer :: j = 2
    end type with_parts

    type :: outer
        type(blah2) :: c
        type(inner) :: d(2)
        type(mid) :: m
        real :: r = 4.0
    end type outer

    type(blah2), parameter :: p = blah2()
    type(blah2) :: hello
    type(outer) :: o
    type(with_parts) :: w
    integer, target :: tgt = 4

    call go()

    hello = blah2(j=5)
    if (hello%b%i /= 1) error stop
    if (hello%j /= 5) error stop

    if (p%b%i /= 1) error stop
    if (p%j /= 2) error stop

    o = outer()
    if (o%c%b%i /= 1) error stop
    if (o%c%j /= 2) error stop
    if (any(o%d%i /= 3)) error stop
    if (o%m%n%i /= 3) error stop
    if (abs(o%r - 4.0) > 1e-6) error stop

    o = outer(r=6.0, m=mid(inner(7, 8)))
    if (o%c%b%i /= 1) error stop
    if (o%c%j /= 2) error stop
    if (any(o%d%i /= 3)) error stop
    if (o%m%n%k /= 7) error stop
    if (o%m%n%i /= 8) error stop
    if (abs(o%r - 6.0) > 1e-6) error stop

    w%q%p => tgt
    w = with_parts(j=7)
    if (associated(w%q%p)) error stop
    if (allocated(w%q%a)) error stop
    if (w%q%s /= "abc") error stop
    if (any(abs(w%q%v - [1.0, 2.0, 3.0]) > 1e-6)) error stop
    if (w%j /= 7) error stop

    call check(blah2(), 1, 2)
    print *, "ok"

contains

    subroutine go()
        type(blah2) :: hello
        hello = blah2()
        if (hello%b%i /= 1) error stop
        if (hello%j /= 2) error stop
    end subroutine go

    subroutine check(x, i, j)
        type(blah2), intent(in) :: x
        integer, intent(in) :: i, j
        if (x%b%i /= i) error stop
        if (x%j /= j) error stop
    end subroutine check

end program structure_constructor_args_31
