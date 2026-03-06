program string_105
implicit none
character(:), allocatable :: a, b, words(:)

a = 'first'
b = 'second'
allocate(character(max(len(a), len(b))) :: words(2))
words(1) = a
words(2) = b

if (len(a) /= 5) error stop
if (len(b) /= 6) error stop
if (len(words(1)) /= 6) error stop
if (len(words(2)) /= 6) error stop
if (words(1) /= 'first ') error stop
if (words(2) /= 'second') error stop
if (size(words) /= 2) error stop
print '(A,2I2)', 'Lengths:', len(words(1)), len(words(2))
end program string_105
