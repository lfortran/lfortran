subroutine parameter_28_explicit(a, r)
integer :: a(3), r
r = a(1) + a(2) + a(3)
end subroutine

subroutine parameter_28_assumed(a, n, r)
integer :: a(*), n, r
r = sum(a(1:n))
end subroutine

subroutine parameter_28_real(a, s)
real :: a(*), s
s = a(1) + a(2) + a(3) + a(4)
end subroutine

subroutine parameter_28_char_explicit(c, r)
character(len=4) :: c(2)
integer :: r
r = 0
if (c(1) == "efgh") r = r + 1
if (c(2) == "ijkl") r = r + 2
end subroutine

subroutine parameter_28_char_assumed(c, r)
character(len=4) :: c(*)
integer :: r
r = 0
if (c(1) == "abcd") r = r + 1
if (c(2) == "efgh") r = r + 2
if (c(3) == "ijkl") r = r + 4
end subroutine

integer function parameter_28_fsum(a)
integer :: a(3)
parameter_28_fsum = a(1) + a(2) + a(3)
end function
