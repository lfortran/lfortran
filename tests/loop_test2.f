c `--show-llvm` throws with
c assert(Ty && "Invalid GetElementPtrInst indices for type!"); 
c 
c -> correct as we do not define lower/upper bounds for arrays here
      subroutine f() 
      integer nq, m
      real wrk5, v
      
      DO 140 L1 = 1,NQ
         DO 110 J = 1,M
            WRK5(J) = V(INDX,J,L1)
  110    CONTINUE
         DO 120 J = 1,M
            VE(INDX,L1,J) = WRK5(J)
  120    CONTINUE
  140 CONTINUE

      indx = 1
      real ve,vev
      real zero
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

