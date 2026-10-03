      MODULE M
        TYPE :: T
          INTEGER, POINTER :: X
        END TYPE T
        TYPE(T) D
      END MODULE M
      SUBROUTINE S(I)
      USE M
      INTEGER, TARGET :: I
      D%X=>I
      END
