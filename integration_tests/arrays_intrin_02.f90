program arrays_intrin_02
  logical l1(3), l2(3), l3(3)
  l1 = (/.true., .true., .true./)
  l2 = (/.true., .true., .false./)
  l3 = (/.false., .false., .false./)
  print *, all(l1), all(l2), all(l3)
  print *, any(l1), any(l2), any(l3)
  call matrixCheck
  contains
    subroutine matrixCheck
      integer m1(2,3), m2(2,3)
      m1 = 1
      m2 = 1
      m2(2,2) = 2
      print *, all(m1 .eq. m2)
      print *, all(m1 .eq. m2, 1)
      print *, all(m1 .eq. m1, 2)
      print *, any(m1 .eq. m2)
      print *, any(m1 .eq. m2, 1)
      print *, any(m1 .eq. m1, 2)
    end subroutine matrixCheck
end program arrays_intrin_02