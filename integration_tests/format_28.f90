program format_28
    implicit none
    integer w,io
    character(12) fmt
    real :: x = -0.0
    real, parameter :: a(4) = [ 1.0, 2.0, 3.0, 4.0 ]
    real, parameter :: b(4) = a / SUM(a)
    print "(4E11.3)", b
    do w = 6,9
      write(fmt,'(A,I0,A)') '(A,ES',w,'.2E1)'
      write(*,fmt,iostat=io) fmt//' ',3e20
      if(io/=0) cycle
    end do
    print "(ES6.2E1)", 3e20
    print "(ES7.2E1)", 3e20
    print "(ES8.2E2)", 3e20
    print "(ES9.2E1)", 3e20
    print "(ES10.2E1)", 3e20
    print "(ES11.2E1)", 3e20
    write(*,"(ES0.0E0)") 0.0
    write(*,"(ES0.0e0)") 10.0
    write(*,"(ES0.0E0)") 3.14159
    write(*,"(ES0.0E0)") 3.14159E+05
    write(*,"(ES0.0E0)") 1.23456789E+10
    write(*,"(ES0.0E0)") -1.23456789E+10
    write(*, "(ES7.3E4)") 1.23456789E+10
    write(*, "(ES7.3E4)") -1.23456789E+10
    write(*,"(ES10.3E2)") 1.23e-40
    write(*,"(ES10.3E2)") -1.23e-40
    write(*,"(ES10.2E1)") 123.45
    write(*,"(ES10.2E2)") 123.45
    write(*,"(ES10.2E3)") 123.45
    write(*,"(ES10.2E2)") 1.0e+2
    write(*,"(ES10.2E2)") 1.0e+0
    write(*,"(ES10.2E2)") 9.995e+0  
    write(*,"(SP,ES10.2E1)") 123.45
    write(*,"(SS,ES10.2E1)") 123.45
    write(*,"(ES4.1E1)") 3.14e+0 
    write(*,"(ES5.1E1)") 3.14e+0
    write(*,"(ES6.1E1)") 0.0
    write(*,"(ES0.0E0)") 0.0
    print '(E7.2)', 289.
    print '(E7.2)', 289.
    print '(E8.2)', 289.
    print '(E7.2)', 0.289
    print '(E9.2)', 1.23456789e+12
    print '(E7.2)', -289.
end program format_28
