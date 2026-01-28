program open_sign_01
    implicit none
    
    ! Test sign='plus' - should show plus sign for positive numbers
    open(6, sign='plus')
    print '(F5.1)', 1.0
    
    ! Don't close unit 6 (stdout) as subsequent prints would have no output
    print *, "PASS"
end program
