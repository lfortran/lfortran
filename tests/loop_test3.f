      PROGRAM TEST
      DO 240 K=1,MP
      CON(K)=-DATMAT(K,NP)
      DO 220 J=1,N
  220 W(J)=DATMAT(K,J)+CON(K)
      DO 240 I=1,N
      TEMP=0.0d0
      DO 230 J=1,N
  230 TEMP=TEMP+W(J)*SIMI(J,I)
      IF (K .EQ. MP) TEMP=-TEMP
  240 A(I,K)=TEMP
      END
