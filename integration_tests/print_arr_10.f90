! Strided pointer array components are formatted element by element
program print_arr_10
implicit none
type :: t
    integer, pointer :: p(:) => null()
    character(len=2), pointer :: c(:) => null()
    real, pointer :: q(:,:) => null()
end type
integer, target :: tg(6) = [1, 2, 3, 4, 5, 6]
character(len=2), target :: ctg(4) = ['aa', 'bb', 'cc', 'dd']
real, target :: rtg(3,4)
type(t) :: s, sa(2)
character(len=40) :: buf
integer :: i, j
do j = 1, 4
    do i = 1, 3
        rtg(i, j) = 10*i + j
    end do
end do
s%p => tg(1:6:2)
s%c => ctg(1:4:2)
s%q => rtg(1:3:2, 2:4:2)
print *, s%p
write(buf, '(3I3)') s%p
if (trim(buf) /= '  1  3  5') error stop 1
print *, s%c
write(buf, '(2A2)') s%c
if (trim(buf) /= 'aacc') error stop 2
print *, s%q
write(buf, '(4F5.0)') s%q
if (trim(buf) /= '  12.  32.  14.  34.') error stop 3
sa(2)%p => tg(6:1:-2)
write(buf, '(3I3)') sa(2)%p
if (trim(buf) /= '  6  4  2') error stop 4
! A contiguous target is formatted in place
s%p => tg(2:4)
write(buf, '(3I3)') s%p
if (trim(buf) /= '  2  3  4') error stop 5
do i = 1, 3
    s%p => tg(i:6:i)
    write(buf, '(6I3)') s%p
    if (i == 2 .and. trim(buf) /= '  2  4  6') error stop 6
end do
end program
