subroutine external_41_sum(a, s)
double precision, intent(inout) :: a(:)
double precision, intent(out) :: s
a = a + 1.0d0
s = sum(a)
end subroutine
