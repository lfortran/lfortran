! GETCWD is a GNU extension. This program unit uses the function form.
program getcwd_function
implicit none
integer :: getcwd, status
intrinsic :: getcwd
character(4096) :: cwd, again
character(1) :: short
character(0) :: empty
cwd = repeat('!', len(cwd))
status = getcwd(cwd)
if (status /= 0 .or. len_trim(cwd) == 0) error stop 1
if (index(cwd, achar(0)) /= 0) error stop 2
again = repeat('!', len(again))
status = getcwd(again)
if (status /= 0 .or. again /= cwd) error stop 3
again = repeat('!', len(again))
status = getcwd(c=again)
if (status /= 0 .or. again /= cwd) error stop 9
again = '!'
status = getcwd(again(2:))
if (status /= 0 .or. again(1:1) /= '!' .or. again(2:) /= trim(cwd)) error stop 6
call exact(cwd, len_trim(cwd))
if (len_trim(cwd) > len(short)) then
    short = '!'
    status = getcwd(short)
    if (status == 0 .or. short /= ' ') error stop 4
end if
status = getcwd(empty)
if (status == 0) error stop 5
print '(a)', trim(cwd)
contains
subroutine exact(expected, n)
integer, intent(in) :: n
character(*), intent(in) :: expected
character(n) :: buffer
integer :: status, getcwd
intrinsic :: getcwd
status = getcwd(buffer)
if (status /= 0 .or. buffer /= trim(expected)) error stop 7
if (index(buffer, achar(0)) /= 0) error stop 8
end subroutine
end program
