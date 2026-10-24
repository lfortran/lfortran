module save_25_f
implicit none
contains
    integer function add_one(x)
        integer, intent(in) :: x
        add_one = x + 1
    end function

    integer function add_two(x)
        integer, intent(in) :: x
        add_two = x + 2
    end function
end module

module save_25_m
use save_25_f, only: add_one, add_two
implicit none
abstract interface
    integer function iface(x)
        integer, intent(in) :: x
    end function
end interface
type :: t
    procedure(iface), pointer, nopass :: p => null()
end type
type :: u
    integer :: n = 0
    procedure(iface), pointer, nopass :: p
end type
type(t) :: ms = t(add_one)
contains
    integer function explicit_save(x)
        integer, intent(in) :: x
        type(t), save :: s = t(add_one)
        explicit_save = s%p(x)
        s%p => add_two
    end function

    integer function implicit_save(x)
        integer, intent(in) :: x
        type(u) :: s = u(10, add_one)
        implicit_save = s%p(x) + s%n
        s%n = s%n + 10
        s%p => add_two
    end function

    integer function block_save(x)
        integer, intent(in) :: x
        block
            type(t), save :: s = t(add_one)
            block_save = s%p(x)
            s%p => add_two
        end block
    end function

    integer function block_implicit_save(x)
        integer, intent(in) :: x
        block
            type(t) :: s = t(add_one)
            block_implicit_save = s%p(x)
            s%p => add_two
        end block
    end function
end module

program save_25
use save_25_f, only: add_two
use save_25_m
implicit none
type(t) :: ps = t(add_two)
integer :: i

if (explicit_save(9) /= 10) error stop 1
if (explicit_save(9) /= 11) error stop 2
if (implicit_save(9) /= 20) error stop 3
if (implicit_save(9) /= 31) error stop 4
do i = 1, 2
    if (block_save(9) /= 9 + i) error stop 5
end do
do i = 1, 2
    if (block_implicit_save(9) /= 9 + i) error stop 6
end do
if (ms%p(1) /= 2) error stop 7
if (ps%p(1) /= 3) error stop 8
print *, "ok"
end program
