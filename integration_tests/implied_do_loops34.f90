program implied_do_loops34
! Test: multiple sibling implied DO loops in formatted write where the first
! loop has zero iterations. The loop variable must be re-initialized correctly
! for each independent implied DO loop.
implicit none
character(len=2) :: a(0), b(2)
integer :: i, n

b(1) = "xx"
b(2) = "yy"
n = 1

! Two sibling implied DO loops sharing loop variable i.
! First loop has zero iterations (size(a) = 0).
! Second loop should still work correctly.
write(*, "(a)") (trim(a(i)), i = 1, size(a)), (trim(b(i)), i = 1, n)

print *, "ok"
end program
