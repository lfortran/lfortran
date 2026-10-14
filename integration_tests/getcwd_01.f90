! GETCWD is a GNU extension. This program unit uses the subroutine form.
program getcwd_subroutine
implicit none
character(4096) :: cwd, again
character(1) :: short
character(0) :: empty
integer :: status
integer(8) :: status8
cwd = repeat('!', len(cwd))
call getcwd(cwd)
if (len_trim(cwd) == 0) error stop 1
if (index(cwd, achar(0)) /= 0) error stop 2
again = repeat('!', len(again))
status = -777
call getcwd(again, status=status)
if (status /= 0 .or. again /= cwd) error stop 3
status8 = -777
call getcwd(again, status8)
if (status8 /= 0 .or. again /= cwd) error stop 4
again = '!'
call getcwd(again(2:), status)
if (status /= 0 .or. again(1:1) /= '!' .or. again(2:) /= trim(cwd)) error stop 9
if (len_trim(cwd) > len(short)) then
    short = '!'
    call getcwd(short, status)
    if (status == 0 .or. short /= ' ') error stop 5
end if
call getcwd(empty, status)
if (status == 0) error stop 6
call exact(cwd, len_trim(cwd))
print '(a)', trim(cwd)
contains
subroutine exact(expected, n)
integer, intent(in) :: n
character(*), intent(in) :: expected
character(n) :: buffer
integer :: status
buffer = repeat('!', n)
call getcwd(buffer, status)
if (status /= 0 .or. buffer /= trim(expected)) error stop 7
if (index(buffer, achar(0)) /= 0) error stop 8
end subroutine
end program
