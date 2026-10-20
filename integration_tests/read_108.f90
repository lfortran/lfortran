program read_108
! Formatted READ of integer(1), integer(2), logical(1), logical(2) and
! logical(8) items stores each element with its own size.
implicit none
integer(1) :: i1(4), g1(2), b1
integer(2) :: i2(4), g2(2), b2
integer(1), allocatable :: a1(:)
integer(2), allocatable :: a2(:)
integer(1), target :: t1(6)
integer(2), target :: t2(3, 2)
integer(1), pointer :: p1(:)
integer(2), pointer :: p2(:, :)
logical(1) :: l1(3), h1(2), s1
logical(2) :: l2(3), h2(2)
logical(8) :: l8(3), h8(2), s8
logical(1), allocatable :: m1(:)
logical(8), allocatable :: m8(:)
logical(8), target :: u8(3, 2)
logical(8), pointer :: q8(:, :)
character(len=20) :: s
integer :: u

s = '  1  2  3  4'

g1 = 9; i1 = 0
read(s, '(4i3)') i1
if (any(i1 /= [1, 2, 3, 4])) error stop 1
if (any(g1 /= 9)) error stop 2

g2 = 9; i2 = 0
read(s, '(4i3)') i2
if (any(i2 /= [1, 2, 3, 4])) error stop 3
if (any(g2 /= 9)) error stop 4

i2 = 0
read(s, 10) i2
10 format(4i3)
if (any(i2 /= [1, 2, 3, 4])) error stop 5

allocate(a1(4), a2(4))
a1 = 0; a2 = 0
read(s, '(4i3)') a1
read(s, '(4i3)') a2
if (any(a1 /= [1, 2, 3, 4])) error stop 6
if (any(a2 /= [1, 2, 3, 4])) error stop 7

t1 = 9; p1 => t1(6:1:-2)
read(s, '(3i3)') p1
if (any(t1 /= [9, 3, 9, 2, 9, 1])) error stop 8

t2 = 9; p2 => t2(1:3:2, :)
read(s, '(4i3)') p2
if (any(t2(:, 1) /= [1, 9, 2]) .or. any(t2(:, 2) /= [3, 9, 4])) error stop 9

i1 = 9
read(s, '(i3)') i1(2)
if (any(i1 /= [9, 1, 9, 9])) error stop 10
i2 = 9
read(s, '(i3)') i2(3)
if (any(i2 /= [9, 9, 1, 9])) error stop 11

g1 = 9; b1 = 0; g2 = 9; b2 = 0
read(s, '(2i3)') b1, b2
if (b1 /= 1 .or. b2 /= 2) error stop 12
if (any(g1 /= 9) .or. any(g2 /= 9)) error stop 13

i1 = 0
s = '0a1f'
read(s, '(2z2)') i1(1:2)
if (i1(1) /= 10 .or. i1(2) /= 31) error stop 14

s = ' T F T'

h1 = .true.; l1 = .false.
read(s, '(3l2)') l1
if (.not. (l1(1) .and. .not. l1(2) .and. l1(3))) error stop 15
if (.not. all(h1)) error stop 16

h2 = .true.; l2 = .false.
read(s, '(3l2)') l2
if (.not. (l2(1) .and. .not. l2(2) .and. l2(3))) error stop 17
if (.not. all(h2)) error stop 18

h8 = .true.; l8 = .false.
read(s, '(3l2)') l8
if (.not. (l8(1) .and. .not. l8(2) .and. l8(3))) error stop 19
if (.not. all(h8)) error stop 20

allocate(m1(3), m8(3))
m1 = .false.; m8 = .false.
read(s, '(3l2)') m1
read(s, '(3l2)') m8
if (.not. (m1(1) .and. .not. m1(2) .and. m1(3))) error stop 21
if (.not. (m8(1) .and. .not. m8(2) .and. m8(3))) error stop 22

u8 = .false.; q8 => u8(1:3:2, :)
s = ' T T F T'
read(s, '(4l2)') q8
if (.not. (u8(1, 1) .and. u8(3, 1) .and. u8(3, 2))) error stop 23
if (u8(2, 1) .or. u8(1, 2) .or. u8(2, 2)) error stop 24

l1 = .false.; l8 = .false.; s1 = .true.; s8 = .false.
s = ' T F T T'
read(s, '(4l2)') l1(2), s1, l8(3), s8
if (.not. (l1(2) .and. l8(3) .and. s8) .or. s1) error stop 25
if (l1(1) .or. l1(3) .or. l8(1) .or. l8(2)) error stop 26

open(newunit=u, status='scratch', form='formatted')
write(u, '(a)') '  5  6  7  8'
write(u, '(a)') ' F T T'
rewind(u)
g2 = 9; i2 = 0
read(u, '(4i3)') i2
if (any(i2 /= [5, 6, 7, 8])) error stop 27
if (any(g2 /= 9)) error stop 28
h1 = .true.; l1 = .true.
read(u, '(3l2)') l1
if (l1(1) .or. .not. (l1(2) .and. l1(3))) error stop 29
if (.not. all(h1)) error stop 30
rewind(u)
i1 = 0
read(u, 20) i1
20 format(4i3)
if (any(i1 /= [5, 6, 7, 8])) error stop 31
close(u)

print *, 'ok'
end program
