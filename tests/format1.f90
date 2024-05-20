program format_04

real :: a,b,c,d,e(6)
double precision :: r,s,t,real_hundred
real, parameter :: t1 = 3.47399991e-03, t2 = 3.47000011e-03
integer :: f,i,j
real(8) :: p,q
a = 123.456
b = 123.45678
c = 12.34
d = 123.45
f = 12345
i = 19
j = 21
r = 12345678
s = 23.5678
t = 0.345678
p = 2.0d0
q = 0.0d0
e = [-1.70138506e+38, -1.25381181e+38, 8.69779800e+37, &
     -1.40706263e+37, 1.11501114e+37, -9.56332244e+37]
real_hundred = 100.0

print *, "ok", "b"
print '(a,a)', "ok", "b"
print '("Success!",/,10X,A6,"World!")',"Hello 123"
print '(4a4)',"dancing","in","the","moonlight"
print '(A2,4(2X,A),I3)',"ab", "cdef", "ghi", "jkl","qwerty",12
print '(i3,i10.5,/i6.6,2x,i3)' , 123,456,12345,6789
print '(d10.2,d15.6,d010.2,2x,d7.2)', 123.456, -123.45678, 12.34, -123.45
print '(1pd10.2,2pd15.6,1pd010.2,2x,1pd9.2)', -a, b, -c, d
print '(-1pe10.2,-2pe15.6,1pe010.2,2x,1pe9.2)', -a, b, -c, d
print "(12(i3))", 1,2,3,4,5,6,7,8,9,10,11,12
print "(4(i3),' hello')", 1,2,3,4,5,6,7,8,9,10,11,12,13,14
print '(i0)', f, -f
print '(d0.0,1x,d0.1,1x,d0.2)',a,b,c
print '(d0.0,1x,d0.1,1x,d0.2)',-a,-b,-c
print '("Hello")'
print '( F13.3,1X,F9.6,1X, F0.2 )', r, s, t
print '( F13.3,1X,F9.6,1X, F0.2 )', -r, -s, -t
print '(1PE13.6)', p, q
print '(F30.25)', 12345e-25
print '("x:", F4.2, " y:", ES7.1)', 1.123, 4.456
print '("x:", ES10.2)', 0.999, 0.1
print '("x:", ES15.5)', 0.102212
print "(*(es15.5e2,1x))", e
! test for issue: https://github.com/lfortran/lfortran/issues/4001
print "(F10.3)", abs(t2-t1)
print "(F10.3)", t2-t1
print "(F0.6)", real_hundred

! test for issue: https://github.com/lfortran/lfortran/issues/4040
print "(2 (I3))", i, j
print "(2 (I 3))", i, j
print 9, i, i + 1, j + 1, i + 2, j + 2
! the below test also ensures that blank character
! isn't removed from ' Dates: '
9 FORMAT (I12, /, ' Dates: ', 2 (2I3, I5))

end program
