program format_98
  ! Test that X descriptor does not overwrite existing characters after TL
  implicit none
  character(len=10) :: buf

  ! Test 1: 1X after TL2 should not overwrite 'C'
  buf = '          '
  write(buf, '(A4,TL2,1X)') 'ABCD'
  if (buf(1:4) /= 'ABCD') error stop

  ! Test 2: 2X after TL3 should not overwrite 'B' or 'C'
  buf = '          '
  write(buf, '(A4,TL3,2X)') 'ABCD'
  if (buf(1:4) /= 'ABCD') error stop

  ! Test 3: X at end (no TL) should extend with space normally
  buf = '          '
  write(buf, '(A2,1X,A2)') 'AB', 'CD'
  if (buf(1:5) /= 'AB CD') error stop

  ! Test 4: TL then write should overwrite (X should not, but A should)
  buf = '          '
  write(buf, '(A4,TL2,A1)') 'ABCD', 'Z'
  if (buf(1:4) /= 'ABZD') error stop

  print *, "PASS"
end program
