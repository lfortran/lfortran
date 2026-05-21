program write_37
! Test list-directed output for integers in a portable way.
!
! The exact output spacing (leading record blank, field width, inter-value
! separator) is processor-dependent: GFortran/LFortran use a 12-char field
! per integer, Flang uses a minimal width. The portable, standard-mandated
! property is that list-directed output is itself valid list-directed input,
! so we round-trip values via an internal read.
implicit none
integer :: i, j, k
character(len=100) :: line

write(line, *) 1, 2
i = 0; j = 0
read(line, *) i, j
if (i /= 1 .or. j /= 2) error stop

write(line, *) 1, 2, 3
i = 0; j = 0; k = 0
read(line, *) i, j, k
if (i /= 1 .or. j /= 2 .or. k /= 3) error stop

write(line, *) 42
i = 0
read(line, *) i
if (i /= 42) error stop

write(line, *) -1, -2
i = 0; j = 0
read(line, *) i, j
if (i /= -1 .or. j /= -2) error stop

write(line, *) 12345
i = 0
read(line, *) i
if (i /= 12345) error stop

print *, "PASS"
end program
