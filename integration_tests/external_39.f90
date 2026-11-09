! An implicitly typed local character is a variable, not the external
! function of the same name defined in this file. s(2:3) is a substring.
character(len=5) function s(i)
integer, intent(in) :: i
s = "hello"
if (i < 0) s = "world"
end function

subroutine sub(res)
implicit character(len=5) (s)
character(len=2), intent(out) :: res
s = "abcde"
if (s(2:3) /= "bc") error stop 1
res = s(4:5)
end subroutine

program external_39
implicit none
character(len=2) :: res
call sub(res)
if (res /= "de") error stop 2
end program
