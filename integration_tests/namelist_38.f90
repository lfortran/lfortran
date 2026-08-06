program namelist_38
! Names in namelist output must be upper case and logical values are
! written as T/F (F2023 13.11.4.1); output must remain readable as input.
implicit none
integer :: u, ios
logical :: l, found_group, found_l
integer :: i
character(len=200) :: line
namelist /nlist/ l, i

l = .true.
i = 789
open(newunit=u, status="scratch", action="readwrite")
write(u, nml=nlist)
rewind(u)
found_group = .false.
found_l = .false.
do
    read(u, "(A)", iostat=ios) line
    if (ios /= 0) exit
    if (index(line, "&NLIST") /= 0) found_group = .true.
    if (index(line, "L=T") /= 0) found_l = .true.
    if (index(line, "&nlist") /= 0) error stop 1
    if (index(line, ".true.") /= 0) error stop 2
end do
if (.not. found_group) error stop 3
if (.not. found_l) error stop 4

! the upper-case output must read back as namelist input
l = .false.
i = 0
rewind(u)
read(u, nml=nlist, iostat=ios)
if (ios /= 0) error stop 5
if (.not. l .or. i /= 789) error stop 6
close(u)

print *, "ok"
end program namelist_38
