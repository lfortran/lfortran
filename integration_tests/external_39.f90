! An implicitly typed local character variable `s` is referenced as a
! substring `s(2:3)` while an external function `s` is defined in the same
! file. The substring is not a function reference, so `s` stays a variable.
character(len=5) function s(i)
integer, intent(in) :: i
s = "hello"
if (i < 0) s = "world"
end function

subroutine sub(res)
implicit character(len=5) (s)
character(len=2), intent(out) :: res
s = "abcde"
print *, s(2:3)
if (s(2:3) /= "bc") error stop 1
res = s(4:5)
end subroutine

program external_39
implicit none
character(len=2) :: res
call sub(res)
print *, res
if (res /= "de") error stop 2
end program
