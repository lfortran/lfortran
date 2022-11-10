      subroutine f()
      integer nq, m
      real wrk5(5), v(3, 4, 5)
      real ve(3, 4, 5),vev(3, 4)
      real zero

      integer l1,j,l2
      integer indx
      DO 140 L1 = 1,NQ
         DO 110 J = 1,M
            WRK5(J) = V(INDX,J,L1)
  110    CONTINUE
         DO 120 J = 1,M
            VE(INDX,L1,J) = WRK5(J)
  120    CONTINUE
  140 CONTINUE

      indx = 1
      zero = 0.0d00
      DO 230 L1 = 1,NQ
         DO 220 L2 = 1,L1
            VEV(L1,L2) = ZERO
            DO 210 J = 1,M
               VEV(L1,L2) = VEV(L1,L2) + VE(INDX,L1,J)*VE(INDX,L2,J)
  210       CONTINUE
            VEV(L2,L1) = VEV(L1,L2)
  220    CONTINUE
  230 CONTINUE

      RETURN
      END

