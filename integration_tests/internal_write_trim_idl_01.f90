module internal_write_trim_idl_01_m
implicit none
contains
function join_fixed(words, nlen) result(text)
character(len=*), intent(in) :: words(:)
integer, intent(in) :: nlen
character(len=size(words)*nlen) :: text
character(len=100) :: fmt_t
integer :: i
write (fmt_t, "('(10000a',i0,')')") nlen
write (text, fmt_t) (trim(words(i)), i=1,size(words))
end function join_fixed
end module internal_write_trim_idl_01_m

program internal_write_trim_idl_01
! Formatted WRITE to an internal file of an implied-do loop of trim()
! values: each trimmed value is a separate output item, edited by its own
! edit descriptor, also when the loop bounds are only known at run time.
use internal_write_trim_idl_01_m, only: join_fixed
implicit none
character(len=4) :: s(2) = ['ab  ', 'cde ']
character(len=4) :: w(2) = ['ab  ', 'c   ']
character(len=20) :: line
character(len=12) :: text
integer :: i, n
n = 2

! Character string edit descriptors between the items.
write (line, '(a,"|",a,"|")') (trim(s(i)), i=1,n)
print '(a)', trim(line)
if (line /= 'ab|cde|') error stop 1

! A field width right-justifies each trimmed value.
write (text, "(2a6)") (trim(w(i)), i=1,n)
print '(a)', '[' // text // ']'
if (text /= '    ab     c') error stop 2

! Run-time format and bounds.
line = join_fixed(['boy ', 'girl', 'a   ', 'tree'], 5)
print '(a)', '[' // line // ']'
if (line /= '  boy girl    a tree') error stop 3

! Sibling items before and after the loop.
write (line, '(a,2(a,"|"),a)') '<', (trim(s(i)), i=1,n), '>'
print '(a)', trim(line)
if (line /= '<ab|cde|>') error stop 4

! Constant bounds.
write (line, '(a,"|",a,"|")') (trim(s(i)), i=1,2)
if (line /= 'ab|cde|') error stop 5
write (text, "(2a6)") (trim(w(i)), i=1,2)
if (text /= '    ab     c') error stop 6

! Plain a edit descriptors concatenate the trimmed values.
write (line, '(*(a))') (trim(s(i)), i=1,n)
print '(a)', trim(line)
if (line /= 'abcde') error stop 7
write (line, '(*(a))') (trim(s(i)), i=1,2)
if (line /= 'abcde') error stop 8

! Zero-trip loop.
n = 0
line = 'x'
write (line, '(*(a))') (trim(s(i)), i=1,n)
if (line /= '') error stop 9
end program internal_write_trim_idl_01
