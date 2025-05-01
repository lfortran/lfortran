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
    print 3, "Hello 123"
    3 FORMAT ("Success!",/,1 0 X , A 6,"World!")
    print '(4a4)',"dancing","in","the","moonlight"
    print 4,"dancing","in","the","moonlight"
    4 FORMAT (4a4)
    print 5,"dancing","in","the","moonlight"
    5 FORMAT (4 a 4)
    print '(A2,4(2X,A),I3)',"ab", "cdef", "ghi", "jkl","qwerty",12
    print 6,"ab", "cdef", "ghi", "jkl","qwerty",12
    6 FORMAT (A 2,4(2 X,A),I3)
    print 7,"ab", "cdef", "ghi", "jkl","qwerty",12
    7 FORMAT (A 2,4 ( 2 X, A),I 3)
    print '(i3,i10.5,/i6.6,2x,i3)' , 123,456,12345,6789
    print 8 , 123,456,12345,6789
    8 FORMAT (i3,i10.5,/i6.6,2 x,i3)
    print 9 , 123,456,12345,6789
    9 FORMAT (i 3,i 10. 5,/i 6 .6, 2 x,i 3)
    print '(d10.2,d15.6,d010.2,2x,d7.2)', 123.456, -123.45678, 12.34, -123.45
    print 10, 123.456, -123.45678, 12.34, -123.45
    10 FORMAT (d 10.2,d 15.6,d 010.2,2x,d 7.2)
    print 11, 123.456, -123.45678, 12.34, -123.45
    11 FORMAT (d 10 .2,d 1 5. 6, d 0 10.2, 2 x, d 7 .2)
    print '(1pd10.2,2pd15.6,1pd010.2,2x,1pd9.2)', -a, b, -c, d
    print 12, -a, b, -c, d
    12 FORMAT (1p d10.2,2 p d15.6,1p d010. 2,2 x,1pd9.2)
    print 13, -a, b, -c, d
    13 FORMAT (1 p d 10.2,2 p d1 5.6,1p d 0 10. 2,2 x,1 p d 9.2)
    print '(-1pe10.2,-2pe15.6,1pe010.2,2x,1pe9.2)', -a, b, -c, d
    print 14, -a, b, -c, d
    14 FORMAT (- 1 p e 10.2,-2 p e 15.6,1 p e 010 . 2, 2 x,1 pe 9.2)
    print 15, -a, b, -c, d
    15 FORMAT (- 1 p e 10.2,-2 p e 15. 6,1 p e 01 0 . 2, 2 x, 1 pe 9. 2)
    print "(12(i3))", 1,2,3,4,5,6,7,8,9,10,11,12
    print 16, 1,2,3,4,5,6,7,8,9,10,11,12
    16 FORMAT ( 12 ( i 3 ))
    print "(4(i3),' hello')", 1,2,3,4,5,6,7,8,9,10,11,12,13,14
    print 17, 1,2,3,4,5,6,7,8,9,10,11,12,13,14
    17 FORMAT (4 ( i 3),' hello')
    print '(i0)', f, -f
    print 18, f, -f
    18 FORMAT (i0)
    print '(d0.0,1x,d0.1,1x,d0.2)',a,b,c
    print 19,a,b,c
    19 FORMAT (d0. 0,1 x,d0.1,1x,d 0.2)
    print 20, a, b, c
    20 FORMAT (d 0. 0,1 x,d 0.1,1x, d 0.2)
    print '(d0.0,1x,d0.1,1x,d0.2)',-a,-b,-c
    print 21,-a,-b,-c
    21 FORMAT (d 0.0,1x,d0.1,1x,d0.2)
    print '("Hello")'
    print '( F13.3,1X,F9.6,1X, F0.2 )', r, s, t
    print 22, r, s, t
    22 FORMAT ( F13.3,1 X,F9.6,1X, F0.2 )
    print 23, r, s, t
    23 FORMAT ( F 13.3,1 X,F 9.6,1 X, F 0. 2 )
    print '( F13.3,1X,F9.6,1X, F0.2 )', -r, -s, -t
    print 24, -r, -s, -t
    24 FORMAT ( F 13.3,1X,F 9.6,1X, F0.2 )
    print '(1PE13.6)', p, q
    print 25, p, q
    25 FORMAT (1PE13.6)
    print '(F30.25)', 12345e-25
    print 26, 12345e-25
    26 FORMAT (F30.25)
    print '("x:", F4.2, " y:", ES7.1)', 1.123, 4.456
    print 27, 1.123, 4.456
    27 FORMAT ("x:", F4.2, " y:", ES7.1)
    print 28, 1.123, 4.456
    28 FORMAT ("x:", F 4. 2, " y:", E S 7.1)
    print '("x:", ES10.2)', 0.999, 0.1
    print 29, 0.999, 0.1
    29 FORMAT ("x:", E S 10.2)
    print '("x:", ES15.5)', 0.102212
    print 30, 0.102212
    30 FORMAT ("x:", ES15.5)
    print "(*(es15.5e2,1x))", e
    print 31, e
    31 FORMAT (*(es15.5e2,1x))
    print 32, e
    32 FORMAT (*( es 15. 5e2, 1 x))
    ! test for issue: https://github.com/lfortran/lfortran/issues/4001
    print "(F10.3)", abs(t2-t1)
    print 33, abs(t2-t1)
    33 FORMAT (F10.3)
    print "(F10.3)", t2-t1
    print 34, t2-t1
    34 FORMAT (F10.3)
    print 35, t2-t1
    35 FORMAT ( F 10. 3)
    print "(F0.6)", real_hundred
    print 36, real_hundred
    36 FORMAT (F10.3)

    ! test for issue: https://github.com/lfortran/lfortran/issues/6348
    print '(A,F0.0,A)', '"',0.0 ,'"' 

    ! test for issue: https://github.com/lfortran/lfortran/issues/4040
    print "(2 (I3))", i, j
    print 37, i, j
    37 FORMAT (I3)
    print "(2 (I 3))", i, j
    print 38, i, j
    38 FORMAT (2 (I3))

    ! test for issue: https://github.com/lfortran/lfortran/issues/6761
    print "(2((I0), 1x))", i, j

    ! test for issue: https://github.com/lfortran/lfortran/issues/6748 
    print "((A))))", ' foobar'

    ! the below test also ensures that blank character
    ! isn't removed from ' Dates: '
    print 39, i, i + 1, j + 1, i + 2, j + 2
    39 FORMAT (I12, /, ' Dates: ', 2 (2I3, I5))
    print 40, i, i + 1, j + 1, i + 2, j + 2
    40 FORMAT (I 1 2, /, ' Dates: ', 2 ( 2 I 3,  I 5  ))
    print 41, i, i + 1, j + 1, i + 2, j + 2
    41 FORMAT (I 12, /, ' Dates: ', 2 (2I3, I5))
    print 42, i, i + 1, j + 1, i + 2, j + 2
    42 FORMAT (I 1 2, /, ' Dates: ', 2 (2 I 3, I 5))
end program
