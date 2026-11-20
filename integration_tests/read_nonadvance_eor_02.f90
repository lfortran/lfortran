program read_nonadvance_eor_02
! The EOR= and END= labels of a non-advancing READ are also taken when its
! FORMAT statement comes after the READ.
implicit none
integer :: u
character(len=10) :: s

open(newunit=u, status='scratch', form='formatted')
write(u, '(a)') 'abc'
write(u, '(a)') 'xyz'
rewind(u)

! EOR= alone
s = '#'
read(u, 100, advance='no', eor=10) s
error stop 'eor= alone: branch not taken'
10 if (s /= 'abc') error stop 'eor= alone: wrong data'

! EOR= with END=: the end of the record is not the end of the file
s = '#'
read(u, 100, advance='no', eor=20, end=90) s
error stop 'eor= with end=: branch not taken'
20 if (s /= 'xyz') error stop 'eor= with end=: wrong data'

! EOR= with END= at the end of the file takes END=
read(u, 100, advance='no', eor=91, end=30) s
error stop 'eor= with end= at end of file: no branch taken'
30 close(u)
print '(a)', 'OK'
stop

90 error stop 'eor= with end=: end= taken instead of eor='
91 error stop 'eor= with end= at end of file: eor= taken instead of end='
100 format(a)
end program
