! Implied-do loop in a list-directed internal read with iostat, used to
! skip items (issue #15946)
module implied_do_loops55_mod
implicit none
contains
subroutine read_date(text,nskip,res)
character(len=*),intent(in) :: text
integer,intent(in) :: nskip
character(len=1),intent(out) :: res
character(len=1) :: dummy
integer :: i,istat
read(text,*,iostat=istat) (dummy,i=1,nskip)
print*,"dummy = ",dummy
if (istat /= 0) error stop 'read failed'
res = dummy
end subroutine
end module

program implied_do_loops55
use implied_do_loops55_mod
implicit none
character(len=1) :: res
call read_date('a b 1 2 2024',2,res)
if (res /= 'b') error stop 1
call read_date('a b 1 2 2024',4,res)
if (res /= '2') error stop 2
call read_date('x',1,res)
if (res /= 'x') error stop 3
end program
