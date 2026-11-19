program allocate_83
! A failed ALLOCATE without STAT= initiates error termination (#17300).
! The request is so large that malloc fails at once, without committing
! any memory. Reaching the end of the program is a failure of this test.
implicit none
real(8), allocatable :: a(:)
integer(8) :: n
n = huge(n) / 16
allocate(a(n))
print *, "execution continued after a failed allocate"
end program
