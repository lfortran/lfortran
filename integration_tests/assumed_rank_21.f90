program assumed_rank_21
implicit none
character(len=8) :: chars(2)
character(len=8) :: written(2), reread(2)
integer :: u

chars = ""
open(newunit=u, status="scratch", action="readwrite")
write(u, '(a)') "'aaa' 'bbb'"
rewind(u)
call read_chars(u, chars)
close(u)
if (chars(1) /= "aaa") error stop 1
if (chars(2) /= "bbb") error stop 2

written = [character(len=8) :: "alpha", "beta"]
reread = ""
open(newunit=u, status="scratch", form="unformatted", action="readwrite")
call write_chars(u, written)
rewind(u)
read(u) reread
close(u)
if (any(reread /= written)) error stop 3

contains

subroutine read_chars(iunit, var)
integer, intent(in) :: iunit
character(len=8), intent(inout) :: var(..)
select rank (var)
rank (1)
    read(iunit, *) var
rank default
    error stop 4
end select
end subroutine

subroutine write_chars(iunit, var)
integer, intent(in) :: iunit
character(len=*), intent(in) :: var(..)
select rank (var)
rank (1)
    write(iunit) var
rank default
    error stop 5
end select
end subroutine

end program
