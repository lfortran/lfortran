! An external subroutine with a `character(len=n)` argument is passed to a
! procedure in another file (implicit_interface_74b.f90) (#12822).
program implicit_interface_74
external s
call lib(s)
print *, "ok"
end program

subroutine s(c, n)
integer :: n
character(len=n) :: c
c = "hello"
end subroutine
