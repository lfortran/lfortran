subroutine other_subroutine(a, n, m)
   integer, intent(in) :: n, m
   real(8), intent(in) :: a(n, m)
   if (a(1,1) /= 1.0d0) error stop
   if (a(2,2) /= 5.0d0) error stop
end subroutine other_subroutine
