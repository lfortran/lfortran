! The frame of a procedure host, which reaches its array through a view. The
! pass prefix runs twice with --cumulative: the passes before nested_vars
! leave alone the aliases that the first run made.
subroutine h(a, n)
integer :: n
real :: a(n)
call inner()
contains
subroutine inner()
a(1) = 5
end subroutine
end subroutine
