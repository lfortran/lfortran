      program two_array_equiv

      implicit none

!

! This creates a 6 integer structure with 

! ia2(3) and ia2(4) overlapping ('unioned') with ia1(1) and ia1(2)

      integer ia1(4), ia2(4)

      equivalence (ia1, ia2(3))

      logical :: pf

      ia1 = [11, 12, 13, 14]

      ia2 = [ 1,  2,  3,  4]  ! 3 and 4 replace 11 and 12 in ia1

      print *, 'ia1 =', ia1

      print *, 'ia2 =', ia2

      pf = all (ia1 == [3, 4, 13, 14]) .and.  &

           all (ia2 == [1, 2,  3,  4])

      print *, 'test: ', merge ('passed', 'failed', pf)

      if (.not. pf) error stop

      end program

