program read_nonadvance_eor_03
! Without END= or IOSTAT=, the end of the file during a non-advancing READ
! with EOR= ends the program with an error, also when its FORMAT statement
! comes after the READ.
implicit none
integer :: u
character(len=10) :: s

open(newunit=u, status='scratch', form='formatted')
write(u, '(a)') 'abc'
rewind(u)
read(u, '(a)') s
read(u, 100, advance='no', eor=10) s
print '(a)', 'end of file did not stop the program'
stop
10 print '(a)', 'eor= taken at the end of the file'
100 format(a)
end program
