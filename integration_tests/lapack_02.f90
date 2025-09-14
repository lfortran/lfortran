C     Minimal reproducer for SGEJSV scalar-to-array issue
C     Error: expected `f32[:]`, passed `f32`
      SUBROUTINE SGEJSV_MINIMAL(M, N, A, LDA, SVA, WORK)
      IMPLICIT NONE
      INTEGER M, N, LDA
      REAL A(LDA, *), SVA(*), WORK(*)
      REAL AAPP, TEMP1, SCALEM
      INTEGER IERR
      EXTERNAL SLASCL

C     The key issue: SLASCL is first called with array element A(1,1)
C     which gets inferred as array type, then called with scalar AAPP
      AAPP = 1.0
      TEMP1 = 2.0
      SCALEM = 0.5

C     FIRST call - line 690 from sgejsv.f - with ARRAY ELEMENT
C     This causes SLASCL's 4th param to be inferred as array
      CALL SLASCL('G', 0, 0, SVA(1), SCALEM, M, 1, A(1,1), LDA, IERR)

C     SECOND call - line 856 from sgejsv.f - with SCALAR
C     This fails because AAPP is scalar but array was expected
      CALL SLASCL('G', 0, 0, AAPP, TEMP1, N, 1, SVA, N, IERR)

C     Third call - line 863 from sgejsv.f
      CALL SLASCL('G', 0, 0, AAPP, TEMP1, M, N, A, LDA, IERR)

      RETURN
      END

      PROGRAM TEST
      IMPLICIT NONE
      INTEGER M, N, LDA
      PARAMETER (M = 10, N = 10, LDA = 10)
      REAL A(LDA, N), SVA(N), WORK(100)

      CALL SGEJSV_MINIMAL(M, N, A, LDA, SVA, WORK)
      PRINT *, 'Done'
      END PROGRAM

C     Dummy SLASCL routine
      SUBROUTINE SLASCL(TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO)
      CHARACTER TYPE
      INTEGER KL, KU, M, N, LDA, INFO
      REAL CFROM, CTO, A(LDA, *)
      RETURN
      END