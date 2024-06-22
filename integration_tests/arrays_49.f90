! This test checks the maxloc intrinsic function when 
! the passed-array dimension is a variable.
SUBROUTINE TEST_MAXLOC(i,a)
  INTEGER,INTENT(in) :: i
  INTEGER, DIMENSION(i),INTENT(in) :: a                                                                    
  INTEGER ::res
  res = maxloc(a,1)
  print *, res
  if(res /= 5) error stop                                                                                                                                    
END SUBROUTINE TEST_MAXLOC

SUBROUTINE TEST_MINLOC(i,a)
  INTEGER,INTENT(in) :: i
  INTEGER, DIMENSION(i),INTENT(in) :: a                                                                    
  INTEGER ::res                                                                
  res = minloc(a,1)
  print *, res
  if(res /= 1) error stop                                                                   
END SUBROUTINE TEST_MINLOC

PROGRAM arrays_49
    implicit none
    INTEGER :: x
    INTEGER :: a(10) = [1,2,3,4,1000,5,6,7,8,9]
    x = 10
    call TEST_MAXLOC(x,a)
    call TEST_MINLOC(x,a)
END PROGRAM arrays_49