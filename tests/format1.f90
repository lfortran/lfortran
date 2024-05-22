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
    print 1, "ok", "b"
    1 FORMAT ( a, a )
    print '("Success!",/,10X,A6,"World!")',"Hello 123"
    print 2, "Hello 123"
    2 FORMAT ("Success!",/,10 X,A 6,"World!")
    print '(4a4)',"dancing","in","the","moonlight"
    print 3,"dancing","in","the","moonlight"
    3 FORMAT (4a4)
    print '(A2,4(2X,A),I3)',"ab", "cdef", "ghi", "jkl","qwerty",12
    print 4,"ab", "cdef", "ghi", "jkl","qwerty",12
    4 FORMAT (A 2,4(2 X,A),I3)
    print '(i3,i10.5,/i6.6,2x,i3)' , 123,456,12345,6789
    print 5 , 123,456,12345,6789
    5 FORMAT (i3,i10.5,/i6.6,2 x,i3)
    print '(d10.2,d15.6,d010.2,2x,d7.2)', 123.456, -123.45678, 12.34, -123.45
    print 6, 123.456, -123.45678, 12.34, -123.45
    6 FORMAT (d 10.2,d 15.6,d 010.2,2x,d 7.2)
    print '(1pd10.2,2pd15.6,1pd010.2,2x,1pd9.2)', -a, b, -c, d
    print 7, -a, b, -c, d
    7 FORMAT (1p d10.2,2 p d15.6,1p d010. 2,2 x,1pd9.2)
    print '(-1pe10.2,-2pe15.6,1pe010.2,2x,1pe9.2)', -a, b, -c, d
    print 8, -a, b, -c, d
    8 FORMAT (- 1 p e 10.2,-2 p e 15.6,1 p e 010 . 2, 2 x,1 pe 9.2)
    print "(12(i3))", 1,2,3,4,5,6,7,8,9,10,11,12
    print 9, 1,2,3,4,5,6,7,8,9,10,11,12
    9 FORMAT ( 12 ( i 3 ))
    print "(4(i3),' hello')", 1,2,3,4,5,6,7,8,9,10,11,12,13,14
    print 10, 1,2,3,4,5,6,7,8,9,10,11,12,13,14
    10 FORMAT (4 ( i 3),' hello')
    print '(i0)', f, -f
    print 11, f, -f
    11 FORMAT (i0)
    print '(d0.0,1x,d0.1,1x,d0.2)',a,b,c
    print 12,a,b,c
    12 FORMAT (d0. 0,1 x,d0.1,1x,d 0.2)
    print '(d0.0,1x,d0.1,1x,d0.2)',-a,-b,-c
    print 13,-a,-b,-c
    13 FORMAT (d 0.0,1x,d0.1,1x,d0.2)
    print '("Hello")'
    print '( F13.3,1X,F9.6,1X, F0.2 )', r, s, t
    print 14, r, s, t
    14 FORMAT ( F13.3,1 X,F9.6,1X, F0.2 )
    print '( F13.3,1X,F9.6,1X, F0.2 )', -r, -s, -t
    print 15, -r, -s, -t
    15 FORMAT ( F 13.3,1X,F 9.6,1X, F0.2 )
    print '(1PE13.6)', p, q
    print 16, p, q
    16 FORMAT (1PE13.6)
    print '(F30.25)', 12345e-25
    print 17, 12345e-25
    17 FORMAT (F30.25)
    print '("x:", F4.2, " y:", ES7.1)', 1.123, 4.456
    print 18, 1.123, 4.456
    18 FORMAT ("x:", F4.2, " y:", ES7.1)
    print '("x:", ES10.2)', 0.999, 0.1
    print 19, 0.999, 0.1
    19 FORMAT ("x:", E S 10.2)
    print '("x:", ES15.5)', 0.102212
    print 20, 0.102212
    20 FORMAT ("x:", ES15.5)
    print "(*(es15.5e2,1x))", e
    print 21, e
    21 FORMAT (*(es15.5e2,1x))
    ! test for issue: https://github.com/lfortran/lfortran/issues/4001
    print "(F10.3)", abs(t2-t1)
    print 22, abs(t2-t1)
    22 FORMAT (F10.3)
    print "(F10.3)", t2-t1
    print 23, t2-t1
    23 FORMAT (F10.3)
    print "(F0.6)", real_hundred
    print 24, real_hundred
    24 FORMAT (F10.3)

    ! test for issue: https://github.com/lfortran/lfortran/issues/4040
    print "(2 (I3))", i, j
    print 25, i, j
    25 FORMAT (I3)
    print "(2 (I 3))", i, j
    print 26, i, j
    26 FORMAT (2 (I3))
    ! the below test also ensures that blank character
    ! isn't removed from ' Dates: '
    print 27, i, i + 1, j + 1, i + 2, j + 2
    27 FORMAT (I 12, /, ' Dates: ', 2 (2I3, I5))
    print 28, i, i + 1, j + 1, i + 2, j + 2
    28 FORMAT (I 12, /, ' Dates: ', 2 (2I3, I5))
    print 29, i, i + 1, j + 1, i + 2, j + 2
    29 FORMAT (I 1 2, /, ' Dates: ', 2 (2 I 3, I 5))
end program
