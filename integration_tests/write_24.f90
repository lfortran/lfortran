program write_24
! Internal WRITE to an allocatable character variable must not
! reallocate. The variable should keep its original allocated length.
implicit none
character(:), allocatable :: s
integer :: n

! Case 1: allocate to length 10, write a short value
allocate(character(len=10) :: s)
write(s, "(i0)") 5
if (len(s) /= 10) error stop
if (s(1:1) /= "5") error stop

! Case 2: write a longer formatted value
s = repeat(" ", 10)
write(s, "(a)") "hello"
if (len(s) /= 10) error stop
if (s(1:5) /= "hello") error stop

! Case 3: write with fixed-width format
s = repeat(" ", 10)
write(s, "(i5)") 42
if (len(s) /= 10) error stop

! Case 4: reallocate to a different length, then write
deallocate(s)
allocate(character(len=20) :: s)
write(s, "(i0)") 12345
if (len(s) /= 20) error stop
if (s(1:5) /= "12345") error stop

print *, "PASS"
end program
