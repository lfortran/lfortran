program arrays_intrin_02
  logical l1(3), l2(3), l3(3)
  integer m1(2,3), m2(2,3)
  l1 = [.true., .true., .true.]
  l2 = [.true., .true., .false.]
  l3 = [.false., .false., .false.]
  if (.not. all(l1)) error stop
  if (all(l2)) error stop
  if (all(l3)) error stop
  if (.not. any(l1)) error stop
  if (.not. any(l2)) error stop
  if (any(l3)) error stop
  m1 = 1
  m2 = 1
  m2(1,2) = 2
  m2(2,2) = 2
  call matrixCheck(m1, m2)
  contains
    subroutine matrixCheck(m1, m2)
      integer, intent(in) :: m1(2,3), m2(2,3)
      if (all(m1 == m2)) error stop
      if (.not. all(all(m1 == m2, 1) .eqv. [.true., .false., .true.])) error stop
      if (.not. all(all(m1 == m2, 2) .eqv. [.false., .false.])) error stop
      if (.not. any(m1 == m2)) error stop
      if (.not. all(any(m1 == m2, 1) .eqv. [.true., .false., .true.])) error stop
      if (.not. all(any(m1 == m2, 2) .eqv. [.true., .true.])) error stop
    end subroutine matrixCheck
end program arrays_intrin_02