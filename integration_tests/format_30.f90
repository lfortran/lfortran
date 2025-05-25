program format_30
    print "(2E13.3)", 1.0, 2.0
    print "(2ES13.3)", epsilon(1e0), epsilon(3e0)
    print "(3F10.4)", 1.2345, 2.3456, 3.4567
    print "(2EN12.3)", 123.0, 456.0
    print "(2ES13.3)", epsilon(1e0), epsilon(3e0)
    print "(2E13.3E2)", epsilon(1e0),epsilon(1d0)
    write(*,"(ES0.0E0)") 1.23456789E+10
    print "(ES0.15)", 1/3.0d-200
    print "(SP,E60.50)", 1.23456789101112e-62_8
    write(*,"(ES0.0E0)") 0.0
    write(*,"(ES0.0E0)") 10.0
    write(*,"(ES0.0E0)") 3.14159
    write(*,"(ES0.0E0)") 3.14159E+05
    print "(E67.62)", 1.23456789101112e-62_8
    print "(ES0.15)", 3.0d-100
end program format_30
