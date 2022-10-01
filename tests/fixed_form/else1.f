      subroutine fun1()
      IF (ABS(BJV0).GT.ABS(BJV1)) CS=BJV0/F
      ELSE CS=BJV1/F2
      end subroutine

      subroutine fun2()
      if (.true.) then
      x = 1
      IF (ABS(BJV0).GT.ABS(BJV1)) CS=BJV0/F
      ELSE CS=BJV1/F2
      end if
      end subroutine

      subroutine fun3()
      if (.true.) then
      x = 1
      IF (ABS(BJV0).GT.ABS(BJV1)) THEN
          CS=BJV0/F
      ELSE
          CS=BJV1/F2
      END IF
      end if
      end subroutine
