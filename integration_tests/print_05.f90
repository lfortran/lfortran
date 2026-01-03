program print_05
implicit none
character(len=4) :: s1(4) = ['sngl', 'dble', 'xten', 'quad']
integer :: i
print *, s1
do i = 1, size(s1)
   print *, i, "|", s1(i), "|"
end do
end program print_05
