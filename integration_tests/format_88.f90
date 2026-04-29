program format_88
! Test that trailing blanks from X edit descriptor are trimmed in output records
implicit none
integer :: u
character(len=100) :: line

open(newunit=u, file="format_88_output.txt", status="replace")

! X descriptor at end of record should produce no trailing blanks
write(u, "(4X)")
! Non-space content followed by X: trailing X blanks should be trimmed
write(u, "('A', 4X)")
! X between content items should still produce spaces (not trailing)
write(u, "('A', 3X, 'B')")
! Multiple content chars followed by trailing X
write(u, "('XY', 2X)")

close(u)

! Read back and verify
open(newunit=u, file="format_88_output.txt", status="old")

! Line 1: only X descriptor -> empty record
read(u, '(A)') line
if (len_trim(line) /= 0) error stop

! Line 2: 'A' followed by 4X -> just 'A' (trailing X trimmed)
read(u, '(A)') line
if (len_trim(line) /= 1) error stop
if (line(1:1) /= 'A') error stop

! Line 3: 'A' + 3X + 'B' -> 5 chars "A   B" (X is not trailing)
read(u, '(A)') line
if (len_trim(line) /= 5) error stop
if (line(1:1) /= 'A') error stop
if (line(5:5) /= 'B') error stop

! Line 4: 'XY' + 2X -> just 'XY' (trailing X trimmed)
read(u, '(A)') line
if (len_trim(line) /= 2) error stop
if (line(1:2) /= 'XY') error stop

close(u, status="delete")

print *, "All tests passed"
end program
