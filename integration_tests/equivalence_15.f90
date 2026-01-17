program equivalence_15
    implicit none
    
    ! Test 1: Integer arrays with offset 3
    integer :: ia1(4), ia2(4)
    equivalence (ia1, ia2(3))
    
    ! Test 2: Real arrays with offset 4
    real :: r1(2), r2(6)
    equivalence (r1, r2(4))
    
    ! Test 3: Complex arrays with offset 2
    complex :: c1(2), c2(3)
    equivalence (c1, c2(2))
    
    ! Test 4: Logical arrays with offset 3
    logical :: l1(2), l2(4)
    equivalence (l1, l2(3))
    
    ! Test 5: Different size arrays with offset 2
    integer :: arr1(3), arr2(5)
    equivalence (arr1, arr2(2))
    
    ! Test 1: Integer equivalence
    ia1 = [11, 12, 13, 14]
    ia2 = [1, 2, 3, 4]
    if (ia1(1) /= 3) error stop "Test 1: ia1(1) should be 3"
    if (ia1(2) /= 4) error stop "Test 1: ia1(2) should be 4"
    if (ia1(3) /= 13) error stop "Test 1: ia1(3) should be 13"
    if (ia1(4) /= 14) error stop "Test 1: ia1(4) should be 14"
    if (ia2(1) /= 1) error stop "Test 1: ia2(1) should be 1"
    if (ia2(3) /= 3) error stop "Test 1: ia2(3) should be 3"
    
    ! Test 2: Real equivalence
    r1 = [1.5, 2.5]
    r2 = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0]
    if (abs(r1(1) - 40.0) > 1.0e-6) error stop "Test 2: r1(1) should be 40.0"
    if (abs(r1(2) - 50.0) > 1.0e-6) error stop "Test 2: r1(2) should be 50.0"
    if (abs(r2(4) - 40.0) > 1.0e-6) error stop "Test 2: r2(4) should be 40.0"
    
    ! Test 3: Complex equivalence
    c1 = [(1.0, 2.0), (3.0, 4.0)]
    c2 = [(10.0, 20.0), (30.0, 40.0), (50.0, 60.0)]
    if (abs(real(c1(1)) - 30.0) > 1.0e-6) error stop "Test 3: c1(1) real should be 30.0"
    if (abs(aimag(c1(1)) - 40.0) > 1.0e-6) error stop "Test 3: c1(1) imag should be 40.0"
    
    ! Test 4: Logical equivalence
    l1 = [.true., .false.]
    l2 = [.false., .false., .true., .true.]
    if (.not. l1(1)) error stop "Test 4: l1(1) should be true"
    if (.not. l1(2)) error stop "Test 4: l1(2) should be true"
    if (.not. l2(3)) error stop "Test 4: l2(3) should be true"
    
    ! Test 5: Different sizes
    arr1 = [100, 200, 300]
    arr2 = [10, 20, 30, 40, 50]
    if (arr1(1) /= 20) error stop "Test 5: arr1(1) should be 20"
    if (arr1(2) /= 30) error stop "Test 5: arr1(2) should be 30"
    if (arr1(3) /= 40) error stop "Test 5: arr1(3) should be 40"
    if (arr2(2) /= 20) error stop "Test 5: arr2(2) should be 20"
    
end program equivalence_15
