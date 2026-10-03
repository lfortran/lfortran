program select_type_38
! Test: pointer assignment to class(*) allocatable holding a derived type,
! then accessing members via select type on the pointer.
implicit none

type :: box
    class(*), allocatable :: value
end type

type :: foo
    integer :: n
end type

type(box), target :: b
class(*), pointer :: p

allocate(b%value, source=foo(42))
p => b%value

select type(p)
type is (foo)
    if (p%n /= 42) error stop
class default
    error stop
end select

print *, "PASS"
end program
