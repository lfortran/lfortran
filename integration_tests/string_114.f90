module string_114_mod
implicit none
contains

subroutine test_self_slice_assumed_len(x)
    character(len=*), intent(in) :: x(:)
    character(len=len(x)), allocatable :: y(:)
    allocate(y(1))
    y(1) = x(1)
    y = y(:1)
    if (y(1) /= "dog") error stop
end subroutine

subroutine test_self_slice_runtime_len(n)
    integer, intent(in) :: n
    character(len=n), allocatable :: y(:)
    allocate(y(2))
    y(1) = "cat"
    y(2) = "dog"
    y = y(:2)
    if (y(1) /= "cat") error stop
    if (y(2) /= "dog") error stop
end subroutine

end module

program string_114
use string_114_mod
implicit none
character(len=3) :: x(1) = ["dog"]

call test_self_slice_assumed_len(x)
call test_self_slice_runtime_len(3)
print *, "PASS"
end program
