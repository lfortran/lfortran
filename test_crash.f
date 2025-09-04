      SUBROUTINE TEST(N, A, LDA, WR, WI, VS, LDVS, WORK)
      IMPLICIT NONE
      INTEGER N, LDA, LDVS, IEVAL
      REAL A(LDA,*), WR(*), WI(*), VS(LDVS,*), WORK(*)
      
      CALL SHSEQR('S', 'V', N, 1, N, A, LDA, WR, WI, VS, LDVS, 
     $            WORK, -1, IEVAL)
      END SUBROUTINE
