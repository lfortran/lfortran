program intrinsics_477
! Lexical character comparisons use the collating sequence: the shorter operand
! is blank-padded on the right to the length of the longer before comparing.
implicit none

! 'A' (len 1) and 'A ' (len 2) are equal after padding to length 2.
if (llt('A', 'A ')) error stop 'llt(A, A )'
if (lgt('A ', 'A')) error stop 'lgt(A , A)'
if (.not. lge('A', 'A ')) error stop 'lge(A, A )'
if (.not. lle('A', 'A ')) error stop 'lle(A, A )'

! 'B' (len 1) and 'B ' (len 2) are equal after padding.
if (lgt('B ', 'B')) error stop 'lgt(B , B)'
if (llt('B', 'B ')) error stop 'llt(B, B )'
if (.not. lge('B ', 'B')) error stop 'lge(B , B)'
if (.not. lle('B', 'B ')) error stop 'lle(B, B )'

print *, "Done"

end program intrinsics_477
