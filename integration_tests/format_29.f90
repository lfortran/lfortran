program format_29
    implicit none
    real :: x

    x = 1.23456
    print '(F10.4)', x
    print '(1PF10.4)', 1.23456
    print '(2PF10.4)', x
    print '(-1PF10.4)', 1.23456
    print '(-3PF10.4)', x

    x = 0.001234
    print '("x=0.001234")'
    print '(F12.6)', x
    print '(3PF12.6)', x

    x = 123456.0
    print '("x=123456.0")'
    print '(F12.2)', x
    print '(-2PF12.2)', x

    x = -9.876
    print '("x=-9.876")'
    print '(0PF10.3)', x
    print '(2PF10.3)', x

    x = 0.1234567
    print '(F8.4)', x
    print '(3PF8.4)', x

    x = 1234567.0
    print '(-2PF8.2)', x

    x = 9.9999
    print '(F8.4)', x
    print '(1PF8.4)', x

    x = 0.00001
    print '(F10.6)', x
    print '(4PF10.6)', x
end program format_29
