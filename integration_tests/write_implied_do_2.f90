program write_implied_do_2
implicit none
integer :: j, m, n, k(3), ios
real :: x(3), y(3)
character(1) :: c(3)
character(100) :: line
x = [1.5, 2.5, 3.5]
c = ['a', 'b', 'c']
n = 3
open(10, status='scratch', action='readwrite')
write(10, "(' x:' / 2(a, i2, f5.1))") (c(j), j, x(j), j = 1, n)
write(10, *) (j, x(j), j = 1, n)
rewind(10)
read(10, '(a)') line
if (line /= ' x:') error stop 'header'
read(10, '(a)') line
if (line /= 'a 1  1.5b 2  2.5') error stop 'first record'
read(10, '(a)') line
if (line /= 'c 3  3.5') error stop 'second record'
read(10, '(a)') line
read(line, *, iostat=ios) k(1), y(1), k(2), y(2), k(3), y(3)
if (ios /= 0) error stop 'list-directed record'
if (any(k /= [1, 2, 3]) .or. any(y /= x)) error stop 'list-directed values'
read(10, '(a)', iostat=ios) line
if (ios >= 0) error stop 'extra record'
close(10)
write(line, '(*(i2, f5.1))') ((j, x(j), j = 1, n), m = 1, 2)
if (line /= ' 1  1.5 2  2.5 3  3.5 1  1.5 2  2.5 3  3.5') error stop 'internal write'
end program
