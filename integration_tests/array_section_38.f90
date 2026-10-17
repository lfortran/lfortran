program array_section_38
! Strided, reversed and rank-2 sections of character arrays written as a
! whole, directly and through an assumed-shape dummy argument
implicit none
character(len=3) :: c(4) = ["aaa", "bbb", "ccc", "ddd"]
character(len=2) :: d(2,3)
character(len=12) :: buf

d = reshape(["11", "21", "12", "22", "13", "23"], [2, 3])

write(buf, '(4a)') c(1:4:2)
if (buf /= "aaaccc") error stop 1
write(buf, '(4a)') c(4:1:-1)
if (buf /= "dddcccbbbaaa") error stop 2
write(buf, '(4a)') d(1,2:3)
if (buf /= "1213") error stop 3

call check(c(1:4:2), "aaaccc", 4)
call check(c(4:1:-1), "dddcccbbbaaa", 5)
call check(d(1,2:3), "1213", 6)

contains

subroutine check(x, expected, code)
character(len=*), intent(in) :: x(:)
character(len=*), intent(in) :: expected
integer, intent(in) :: code
character(len=12) :: b
write(b, '(4a)') x
if (b /= expected) error stop code
end subroutine

end program
