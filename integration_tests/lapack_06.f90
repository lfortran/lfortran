C MRE for SGESVD
      SUBROUTINE SGESVD( S, LDA, M, N, ANRM, SMLNUM, IERR )
      INTEGER LDA, M, N, IERR
      REAL S( * ), ANRM, SMLNUM
      CALL SLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, S, LDA, IERR )
      END

      SUBROUTINE SLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
      CHARACTER TYPE
      INTEGER KL, KU, M, N, LDA, INFO
      REAL CFROM, CTO
      REAL A( LDA, * )
      A(1,1) = CFROM
      END

      PROGRAM LAPACK_06
      REAL S(10)
      REAL ANRM, SMLNUM
      INTEGER LDA, M, N, IERR
      LDA = 10
      M = 5
      N = 5
      ANRM = 1.0
      SMLNUM = 0.1
      IERR = 0
      CALL SGESVD( S, LDA, M, N, ANRM, SMLNUM, IERR )
      PRINT *, "OK"
      END
