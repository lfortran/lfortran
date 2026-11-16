program string_substring_int8_alloc_bounds_01
! Substrings whose bounds are allocatable or pointer integer(8) scalars
implicit none
integer(8), allocatable :: i8, j8
integer(8), pointer :: p8
integer(8), target :: t8
character(len=10) :: s
allocate(i8, j8)
i8 = 3_8; j8 = 5_8
t8 = 2_8; p8 => t8
s = "abcdefghij"
print *, s(i8:j8)
if (s(i8:j8) /= "cde") error stop 1
print *, s(p8:j8)
if (s(p8:j8) /= "bcde") error stop 2
end program
