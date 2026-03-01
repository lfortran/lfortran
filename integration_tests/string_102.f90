program string_102
! Test character array slice assignment where the second array has
! character(len=len(first_array)) type and first array is assumed-length.
implicit none
character(len=5) :: a(4), b(4)
a = "hello"
b = "world"
call test(a, b)
if (a(1) /= "world") error stop
if (a(2) /= "world") error stop
if (a(3) /= "hello") error stop
if (a(4) /= "hello") error stop
print *, "ok"
contains
    subroutine test(a, b)
        character(len=*), intent(inout) :: a(:)
        character(len=len(a)), intent(inout) :: b(:)
        a(1:2) = b(1:2)
    end subroutine
end program
