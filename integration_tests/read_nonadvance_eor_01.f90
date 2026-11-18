program read_nonadvance_eor_01
! A non-advancing READ that hits the end of a record branches to its EOR=
! label; END= is taken only at the end of the file and ERR= only on errors.
use, intrinsic :: iso_fortran_env, only: iostat_eor
implicit none
integer :: u, ios, n
character(len=10) :: s

open(newunit=u, status='scratch', form='formatted')
write(u, '(a)') 'abc'
write(u, '(a)') 'xyz'
write(u, '(a)') 'pq'
write(u, '(a)') 'mn'
write(u, '(a)') 'uvw'
rewind(u)

! EOR= alone
s = '#'
read(u, '(a)', advance='no', eor=10) s
error stop 'eor= alone: branch not taken'
10 if (s /= 'abc') error stop 'eor= alone: wrong data'

! EOR= with END=: the end of the record is not the end of the file
s = '#'
read(u, '(a)', advance='no', eor=20, end=90) s
error stop 'eor= with end=: branch not taken'
20 if (s /= 'xyz') error stop 'eor= with end=: wrong data'

! EOR= with IOSTAT= and SIZE=
s = '#'
ios = 0
n = -1
read(u, '(a)', advance='no', eor=30, iostat=ios, size=n) s
error stop 'eor= with iostat=: branch not taken'
30 if (ios /= iostat_eor) error stop 'eor= with iostat=: wrong iostat'
if (n /= 2) error stop 'eor= with iostat=: wrong size'
if (s /= 'pq') error stop 'eor= with iostat=: wrong data'

! EOR= with ERR=: the end of a record is not an error
s = '#'
read(u, '(a)', advance='no', eor=40, err=91) s
error stop 'eor= with err=: branch not taken'
40 if (s /= 'mn') error stop 'eor= with err=: wrong data'

! IOSTAT= with END= but no EOR=: END= is not taken at the end of a record
s = '#'
ios = 0
read(u, '(a)', advance='no', iostat=ios, end=92) s
if (ios /= iostat_eor) error stop 'iostat= with end=: wrong iostat'
if (s /= 'uvw') error stop 'iostat= with end=: wrong data'

! EOR= with END= at the end of the file takes END=
read(u, '(a)', advance='no', eor=93, end=50) s
error stop 'eor= with end= at end of file: no branch taken'
50 continue

! END= alone at the end of the file
rewind(u)
read(u, '(a)') s
read(u, '(a)') s
read(u, '(a)') s
read(u, '(a)') s
read(u, '(a)') s
read(u, '(a)', advance='no', end=60) s
error stop 'end= alone: branch not taken'
60 close(u)
print '(a)', 'OK'
stop

90 error stop 'eor= with end=: end= taken instead of eor='
91 error stop 'eor= with err=: err= taken instead of eor='
92 error stop 'iostat= with end=: end= taken at the end of a record'
93 error stop 'eor= with end= at end of file: eor= taken instead of end='
end program
