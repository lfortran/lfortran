program print_15
! Verify the runtime list-directed (`print *, ...`) output uses minimal
! widths and emits no leading whitespace by default. This is the
! Flang-compatible behavior. The opt-in `--print-leading-space` flag (and
! `--std=f23` / `--std=legacy`) inserts an explicit leading space at the
! semantics layer; that is intentionally NOT exercised here, since this
! test runs in default mode.
!
! GFortran emits Gw.d-style padded fields for list-directed output, so
! the exact-text assertions below are LFortran/Flang specific. Keep this
! test labeled `llvm` only.
implicit none
character(len=200) :: line
integer :: i, j
real :: r
real(8) :: d
logical :: l
complex :: c

! Single positive integer: minimal width, no leading whitespace.
i = 5
write(line, *) i
if (line(1:2) /= "5 ") error stop "single positive integer"

! Single negative integer: no leading whitespace, '-' is the first char.
j = -7
write(line, *) j
if (line(1:3) /= "-7 ") error stop "single negative integer"

! Two integers: separated by exactly one space.
write(line, *) j, i
if (line(1:5) /= "-7 5 ") error stop "two integers"

! Three integers (mix of signs).
write(line, *) i, j, i
if (line(1:8) /= "5 -7 5 ") error stop "three integers"

! Logical: minimal 'T' / 'F', no leading whitespace.
l = .true.
write(line, *) l
if (line(1:2) /= "T ") error stop "logical"

! Real: no leading whitespace, no trailing pad-to-width.
r = 1.5
write(line, *) r
if (line(1:11) /= "1.50000000 ") error stop "single real(4)"

! Real(8): no leading whitespace.
d = 1.5d0
write(line, *) d
if (line(1:19) /= "1.5000000000000000 ") error stop "single real(8)"

! Complex: no leading whitespace, components separated by ','.
c = (1.0, 2.0)
write(line, *) c
if (line(1:24) /= "(1.00000000,2.00000000) ") error stop "complex"

! Character literal followed by integer: literal preserved as-is, then a
! single separator and the minimal-width integer.
write(line, *) "x is ", i
if (line(1:8) /= "x is  5 ") error stop "char + int"

! Mixed integer and real.
write(line, *) i, r
if (line(1:13) /= "5 1.50000000 ") error stop "int + real"

print *, "All minimal-width list-directed output tests passed."
end program
