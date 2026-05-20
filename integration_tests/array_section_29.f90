module array_section_29_mod
implicit none
contains

subroutine foo(ivec, ipos)
    character(len=*), intent(in) :: ivec(:)
    integer, intent(in) :: ipos(:)
    logical :: cmp(2)
    print *, ivec(ipos(:2))
    cmp = ivec(3) /= ivec(ipos(:2))
    if (.not. all(cmp .eqv. [.false., .true.])) error stop
end subroutine

end module

program array_section_29
use array_section_29_mod, only: foo
implicit none

character(len=3) :: x(5)
integer :: ipos(5)

x = [character(len=3) :: "aa", "bb", "aa", "cc", "dd"]
ipos = [1, 2, 3, 4, 5]

call foo(x, ipos)
end program
