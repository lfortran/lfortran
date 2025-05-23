! tests for 'EN' formatting
program format3
    ! standard positive values
    real :: num1, num2, num3, small_num4, under_power_10
    ! standard negative values
    real :: neg_num1, small_neg_num4
    real :: zero1
    num1 = 12345.6789
    num2 = 0.000123456789
    num3 = 1234.5678
    small_num4 = 0.000123456789
    under_power_10 = 999.9999

    neg_num1 = -1234.5678
    small_neg_num4 = -0.000123456789
    zero1 = 0.0

    print '(EN12.4)', num1
    print '(EN12.4)', num2
    print '(EN10.4)', num3
    print '(EN10.4)', small_num4
    print '(EN10.4)', under_power_10

    print '(EN12.4)', neg_num1
    print '(EN15.5)', small_neg_num4

    print '(EN10.4)', zero1

end program format3
