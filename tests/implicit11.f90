integer function f1(z,i)
implicit complex*16 (z,i)
z = 5
print *, dimag(z), dimag(i)
end function

subroutine f2(z,i)
implicit complex*16 (z,i)
z = 5
! Does not yet work due to: https://github.com/lfortran/lfortran/issues/894
!print *, dimag(z), dimag(i)
end subroutine
