program format_04

real :: a,b,c,d
a = 123.456
b = 123.45678
c = 12.34
d = 123.45

print *, "ok", "b"
print '(a,a)', "ok", "b"
print '("Success!",/,10X,A6,"World!")',"Hello 123"
print '(4a4)',"dancing","in","the","moonlight"
print '(A2,4(2X,A),I3)',"ab", "cdef", "ghi", "jkl","qwerty",12
print '(i3,i10.5,/i6.6,2x,i3)' , 123,456,12345,6789
print '(d10.2,d15.6,d010.2,2x,d7.2)', 123.456, -123.45678, 12.34, -123.45
print '(1pd10.2,2pd15.6,1pd010.2,2x,1pd9.2)', -a, b, -c, d
print '(-1pe10.2,-2pe15.6,1pe010.2,2x,1pe9.2)', -a, b, -c, d
print "(9(i3))", 1,2,3,4,5,6,7,8,9
print "(12(i3))", 1,2,3,4,5,6,7,8,9,10,11,12
end program
