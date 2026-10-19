! Strided character arrays whose length is not a constant are formatted
! element by element
program print_arr_16
implicit none
character(len=:), pointer :: cp(:)
character(len=4), target :: ct(3) = ['ab  ', 'cde ', 'fghi']
character(len=2), target :: dt(4) = ['p1', 'q2', 'r3', 's4']
character(len=40) :: buf
cp => dt(2:4:2)
print *, cp
write(buf, '(*(a,"|"))') cp
if (trim(buf) /= 'q2|s4|') error stop 1
cp => ct(3:1:-2)
write(buf, '(*(a,"|"))') cp
if (trim(buf) /= 'fghi|ab  |') error stop 2
! A contiguous target is formatted in place
cp => dt(2:3)
write(buf, '(*(a,"|"))') cp
if (trim(buf) /= 'q2|r3|') error stop 3
call explicit_length(3, ['abc', 'def', 'ghi', 'jkl'])
call assumed_length(ct(3:1:-2), 'fghi|ab  |')
call assumed_length(ct, 'ab  |cde |fghi|')
contains
subroutine explicit_length(m, c)
    integer, intent(in) :: m
    character(len=m), intent(in) :: c(:)
    character(len=m), allocatable, target :: cc(:)
    character(len=m), pointer :: q(:)
    character(len=40) :: b
    allocate(cc(size(c)))
    cc = c
    q => cc(4:1:-3)
    write(b, '(*(a,"|"))') q
    if (trim(b) /= 'jkl|abc|') error stop 4
    write(b, '(*(a,"|"))') c(::2)
    if (trim(b) /= 'abc|ghi|') error stop 5
end subroutine
subroutine assumed_length(a, expected)
    character(len=*), intent(in) :: a(:), expected
    character(len=40) :: b
    write(b, '(*(a,"|"))') a
    if (trim(b) /= expected) error stop 6
end subroutine
end program
