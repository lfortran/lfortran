program equivalence_14
  implicit none
  
  call log_equiv()
  call comp_equiv_8913()

contains

  subroutine log_equiv()
    logical :: la(2)
    logical :: l1
    equivalence (la(1), l1)
    
    l1 = .true.
    la(2) = .false.
    if (.not. la(1)) error stop "la(1) should be true (equivalenced to l1)"
    if (.not. l1) error stop "l1 should be true"
    if (la(2)) error stop "la(2) should be false"
    
    la(1) = .false.
    if (l1) error stop "l1 should be false after la(1) change"
  end subroutine

  subroutine comp_equiv_8913()
    implicit none
    complex :: c1
    real :: r1(6)
    equivalence (c1, r1(1))
    
    r1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    if (abs(real(c1) - 1.0) > 1e-6) error stop "real(c1) should be 1.0"
    if (abs(aimag(c1) - 2.0) > 1e-6) error stop "aimag(c1) should be 2.0"
    
    c1 = (7.0, 8.0)
    if (abs(r1(1) - 7.0) > 1e-6) error stop "r1(1) should be 7.0 after c1 change"
    if (abs(r1(2) - 8.0) > 1e-6) error stop "r1(2) should be 8.0 after c1 change"
    if (abs(r1(3) - 3.0) > 1e-6) error stop "r1(3) should still be 3.0"
  end subroutine

end program equivalence_14
