program format_06

real :: a,b,c,d
a = 123.456
b = 123.45678
c = 12.34
d = 123.45

10 format(a,a)
20 format("Success!",/,10X,A6,"World!")
30 format(4a4)
40 format(A2,4(2X,A),I3)
50 format(i3,i10.5,/i6.6,2x,i3)
60 format(d10.2,d15.6,d010.2,2x,d7.2)
70 format(1pd10.2,2pd15.6,1pd010.2,2x,1pd9.2)
80 format(-1pe10.2,-2pe15.6,1pe010.2,2x,1pe9.2)

print *, "ok", "b"
print 10, "ok", "b"
print 20, "Hello 123"
print 30, "dancing","in","the","moonlight"
print 40, "ab", "cdef", "ghi", "jkl","qwerty",12
print 50 , 123,456,12345,6789
print 60, 123.456, -123.45678, 12.34, -123.45
print 70, -a, b, -c, d
print 80, -a, b, -c, d

end program
