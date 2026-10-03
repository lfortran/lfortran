program intrinsics_478
! LGE/LGT/LLE/LLT compare character operands using the collating sequence
! after the shorter operand is blank-padded on the right to the length of the
! longer one. This must hold for compile time (folded) operands as well as for
! operands that are only known at run time, and it differs from comparing the
! two operands with their trailing blanks removed whenever the longer operand
! carries characters that collate below a blank.
implicit none

character(len=3) :: short_abc
character(len=5) :: abc_blanks
character(len=2) :: ab
character(len=0) :: empty
character(len=3) :: blanks
character(len=5) :: abc_low
character(len=5) :: abc_high

short_abc = 'abc'
abc_blanks = 'abc  '
ab = 'ab'
blanks = '   '
abc_low = 'abc'//char(0)//char(1)
abc_high = 'abc'//char(126)//char(126)

! ---------------------------------------------------------------------------
! Compile time operands (constant folded)
! ---------------------------------------------------------------------------

! Trailing blanks do not change the value of a character operand.
if (.not. lge('abc', 'abc  ')) error stop 'lge(abc, abc  )'
if (.not. lle('abc', 'abc  ')) error stop 'lle(abc, abc  )'
if (lgt('abc', 'abc  ')) error stop 'lgt(abc, abc  )'
if (llt('abc', 'abc  ')) error stop 'llt(abc, abc  )'

! The longer operand wins when its extra characters collate above a blank.
if (.not. lgt('abc', 'ab')) error stop 'lgt(abc, ab)'
if (.not. lge('abc', 'ab')) error stop 'lge(abc, ab)'
if (lle('abc', 'ab')) error stop 'lle(abc, ab)'
if (llt('abc', 'ab')) error stop 'llt(abc, ab)'

! The same comparison with the operands swapped.
if (.not. llt('ab', 'abc')) error stop 'llt(ab, abc)'
if (.not. lle('ab', 'abc')) error stop 'lle(ab, abc)'
if (lgt('ab', 'abc')) error stop 'lgt(ab, abc)'
if (lge('ab', 'abc')) error stop 'lge(ab, abc)'

! A zero length operand equals a string of blanks.
if (.not. lge('', '   ')) error stop 'lge(, blanks)'
if (.not. lle('', '   ')) error stop 'lle(, blanks)'
if (lgt('', '   ')) error stop 'lgt(, blanks)'
if (llt('', '   ')) error stop 'llt(, blanks)'
if (.not. lgt('a', '')) error stop 'lgt(a, )'
if (.not. llt('', 'a')) error stop 'llt(, a)'

! Characters that collate below a blank make the longer operand the smaller
! one, which blank padding gets right and trimming does not.
if (.not. lgt('abc', 'abc'//char(1)//char(1))) error stop 'lgt(abc, abc low)'
if (lle('abc', 'abc'//char(1)//char(1))) error stop 'lle(abc, abc low)'
if (.not. llt('abc'//char(1), 'abc')) error stop 'llt(abc low, abc)'
if (.not. lgt('abc'//char(126), 'abc')) error stop 'lgt(abc high, abc)'

! ---------------------------------------------------------------------------
! Run time operands (variables)
! ---------------------------------------------------------------------------

if (.not. lge(short_abc, abc_blanks)) error stop 'lge(short_abc, abc_blanks)'
if (.not. lle(short_abc, abc_blanks)) error stop 'lle(short_abc, abc_blanks)'
if (lgt(short_abc, abc_blanks)) error stop 'lgt(short_abc, abc_blanks)'
if (llt(short_abc, abc_blanks)) error stop 'llt(short_abc, abc_blanks)'

if (.not. lgt(short_abc, ab)) error stop 'lgt(short_abc, ab)'
if (.not. lge(short_abc, ab)) error stop 'lge(short_abc, ab)'
if (lle(short_abc, ab)) error stop 'lle(short_abc, ab)'
if (llt(short_abc, ab)) error stop 'llt(short_abc, ab)'

if (.not. llt(ab, short_abc)) error stop 'llt(ab, short_abc)'
if (.not. lle(ab, short_abc)) error stop 'lle(ab, short_abc)'
if (lgt(ab, short_abc)) error stop 'lgt(ab, short_abc)'
if (lge(ab, short_abc)) error stop 'lge(ab, short_abc)'

if (.not. lge(empty, blanks)) error stop 'lge(empty, blanks)'
if (.not. lle(empty, blanks)) error stop 'lle(empty, blanks)'
if (lgt(empty, blanks)) error stop 'lgt(empty, blanks)'
if (llt(empty, blanks)) error stop 'llt(empty, blanks)'
if (.not. lgt(short_abc, empty)) error stop 'lgt(short_abc, empty)'
if (.not. llt(empty, short_abc)) error stop 'llt(empty, short_abc)'

if (.not. lgt(short_abc, abc_low)) error stop 'lgt(short_abc, abc_low)'
if (.not. lge(short_abc, abc_low)) error stop 'lge(short_abc, abc_low)'
if (lle(short_abc, abc_low)) error stop 'lle(short_abc, abc_low)'
if (llt(short_abc, abc_low)) error stop 'llt(short_abc, abc_low)'

if (.not. llt(abc_low, short_abc)) error stop 'llt(abc_low, short_abc)'
if (.not. lle(abc_low, short_abc)) error stop 'lle(abc_low, short_abc)'
if (lgt(abc_low, short_abc)) error stop 'lgt(abc_low, short_abc)'
if (lge(abc_low, short_abc)) error stop 'lge(abc_low, short_abc)'

if (.not. lgt(abc_high, short_abc)) error stop 'lgt(abc_high, short_abc)'
if (.not. llt(short_abc, abc_high)) error stop 'llt(short_abc, abc_high)'

! The relational operators follow the same padding rule.
if (.not. (short_abc == abc_blanks)) error stop 'short_abc == abc_blanks'
if (short_abc == ab) error stop 'short_abc == ab'
if (short_abc == abc_low) error stop 'short_abc == abc_low'
if (.not. (short_abc > abc_low)) error stop 'short_abc > abc_low'
if (.not. (short_abc < abc_high)) error stop 'short_abc < abc_high'

print *, "Done"

end program intrinsics_478
