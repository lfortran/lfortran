program complex_implicit_cast_test
    implicit none
    
    ! Test variables for different kinds and types
    complex(4) :: c4_1, c4_2
    complex(8) :: c8_1, c8_2
    real(4) :: r4_1, r4_2
    real(8) :: r8_1, r8_2
    integer :: i1, i2
    
    ! Initialize test values
    c4_1 = (1.5, 2.5)
    c8_1 = (3.5d0, 4.5d0)
    r4_1 = 5.5
    r8_1 = 6.5d0
    i1 = 7
    
    print *, "=== Complex Implicit Casting Tests ==="
    
    ! Test 1: Integer to Complex casting
    print *, "Test 1: Integer to Complex"
    c4_2 = i1  ! IntegerToComplex
    c8_2 = i1  ! IntegerToComplex
    print *, "c4_2 from integer:", c4_2
    print *, "c8_2 from integer:", c8_2
    if (abs(real(c4_2) - 7.0) > 1e-5) error stop "Integer to complex(4) real part failed"
    if (abs(aimag(c4_2) - 0.0) > 1e-5) error stop "Integer to complex(4) imag part failed"
    if (abs(real(c8_2) - 7.0d0) > 1e-10) error stop "Integer to complex(8) real part failed"
    if (abs(aimag(c8_2) - 0.0d0) > 1e-10) error stop "Integer to complex(8) imag part failed"
    
    ! Test 2: Real to Complex casting
    print *, "Test 2: Real to Complex"
    c4_2 = r4_1  ! RealToComplex
    c8_2 = r8_1  ! RealToComplex
    print *, "c4_2 from real(4):", c4_2
    print *, "c8_2 from real(8):", c8_2
    if (abs(real(c4_2) - 5.5) > 1e-5) error stop "Real(4) to complex(4) real part failed"
    if (abs(aimag(c4_2) - 0.0) > 1e-5) error stop "Real(4) to complex(4) imag part failed"
    if (abs(real(c8_2) - 6.5d0) > 1e-10) error stop "Real(8) to complex(8) real part failed"
    if (abs(aimag(c8_2) - 0.0d0) > 1e-10) error stop "Real(8) to complex(8) imag part failed"
    
    ! Test 3: Complex to Real casting (takes real part)
    print *, "Test 3: Complex to Real"
    r4_2 = c4_1  ! ComplexToReal
    r8_2 = c8_1  ! ComplexToReal
    print *, "r4_2 from complex(4):", r4_2
    print *, "r8_2 from complex(8):", r8_2
    if (abs(r4_2 - 1.5) > 1e-5) error stop "Complex(4) to real(4) failed"
    if (abs(r8_2 - 3.5d0) > 1e-10) error stop "Complex(8) to real(8) failed"
    
    ! Test 4: Complex to Integer casting (takes real part)
    print *, "Test 4: Complex to Integer"
    i2 = c4_1  ! ComplexToInteger
    print *, "i2 from complex(4):", i2
    if (i2 /= 1) error stop "Complex(4) to integer failed"
    
    ! Test 5: Complex kind conversion (ComplexToComplex)
    print *, "Test 5: Complex kind conversion"
    c8_2 = c4_1  ! ComplexToComplex (4 to 8)
    c4_2 = c8_1  ! ComplexToComplex (8 to 4)
    print *, "c8_2 from complex(4):", c8_2
    print *, "c4_2 from complex(8):", c4_2
    if (abs(real(c8_2) - 1.5d0) > 1e-10) error stop "Complex(4) to complex(8) real part failed"
    if (abs(aimag(c8_2) - 2.5d0) > 1e-10) error stop "Complex(4) to complex(8) imag part failed"
    if (abs(real(c4_2) - 3.5) > 1e-5) error stop "Complex(8) to complex(4) real part failed"
    if (abs(aimag(c4_2) - 4.5) > 1e-5) error stop "Complex(8) to complex(4) imag part failed"
    
    ! Test 6: Cross-kind casting with ComplexRe and ComplexIm
    print *, "Test 6: ComplexRe/ComplexIm with kind conversion"
    r8_2 = c4_1%re  ! ComplexRe with RealToReal cast (4 to 8)
    r4_2 = c8_1%re  ! ComplexRe with RealToReal cast (8 to 4)
    print *, "r8_2 from c4_1%re:", r8_2
    print *, "r4_2 from c8_1%re:", r4_2
    if (abs(r8_2 - 1.5d0) > 1e-10) error stop "ComplexRe real(4) to real(8) failed"
    if (abs(r4_2 - 3.5) > 1e-5) error stop "ComplexRe real(8) to real(4) failed"
    
    r8_2 = c4_1%im  ! ComplexIm with RealToReal cast (4 to 8)
    r4_2 = c8_1%im  ! ComplexIm with RealToReal cast (8 to 4)
    print *, "r8_2 from c4_1%im:", r8_2
    print *, "r4_2 from c8_1%im:", r4_2
    if (abs(r8_2 - 2.5d0) > 1e-10) error stop "ComplexIm real(4) to real(8) failed"
    if (abs(r4_2 - 4.5) > 1e-5) error stop "ComplexIm real(8) to real(4) failed"
    
    ! Test 7: Complex constructor with mixed types (implicit casting)
    print *, "Test 7: Complex constructor with mixed types"
    c4_2 = cmplx(i1, r4_1)      ! Integer and real(4) to complex(4)
    c8_2 = cmplx(r8_1, i1)      ! Real(8) and integer to complex(8)
    print *, "c4_2 from cmplx(int, real4):", c4_2
    print *, "c8_2 from cmplx(real8, int):", c8_2
    if (abs(real(c4_2) - 7.0) > 1e-5) error stop "cmplx(int, real4) real part failed"
    if (abs(aimag(c4_2) - 5.5) > 1e-5) error stop "cmplx(int, real4) imag part failed"
    if (abs(real(c8_2) - 6.5d0) > 1e-10) error stop "cmplx(real8, int) real part failed"
    if (abs(aimag(c8_2) - 7.0d0) > 1e-10) error stop "cmplx(real8, int) imag part failed"
    
    ! Test 8: Array operations with implicit casting
    print *, "Test 8: Array operations with implicit casting"
    block
        complex(4) :: c4_arr(2) = [(1.0, 2.0), (3.0, 4.0)]
        complex(8) :: c8_arr(2)
        real(8) :: r8_arr(2)
        
        c8_arr = c4_arr  ! ComplexToComplex array casting
        r8_arr = c4_arr%re  ! ComplexRe with RealToReal array casting
        
        print *, "c8_arr from c4_arr:", c8_arr
        print *, "r8_arr from c4_arr%re:", r8_arr
        
        if (abs(real(c8_arr(1)) - 1.0d0) > 1e-10) error stop "Array complex(4) to complex(8) [1] real failed"
        if (abs(aimag(c8_arr(1)) - 2.0d0) > 1e-10) error stop "Array complex(4) to complex(8) [1] imag failed"
        if (abs(real(c8_arr(2)) - 3.0d0) > 1e-10) error stop "Array complex(4) to complex(8) [2] real failed"
        if (abs(aimag(c8_arr(2)) - 4.0d0) > 1e-10) error stop "Array complex(4) to complex(8) [2] imag failed"
        
        if (abs(r8_arr(1) - 1.0d0) > 1e-10) error stop "Array ComplexRe real(4) to real(8) [1] failed"
        if (abs(r8_arr(2) - 3.0d0) > 1e-10) error stop "Array ComplexRe real(4) to real(8) [2] failed"
    end block
    
    print *, "=== All Complex Implicit Casting Tests Passed! ==="
end program