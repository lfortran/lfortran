      PROGRAM TEST
      DO 240 K=1+1/2-3**4,MP
      CON(K)=-DATMAT(K,NP)
      DO 220 J=f()/2,N
  220 W(J)=DATMAT(K,J)+CON(K)
      DO 240 I=f(2,3,x=5,z="o""k'(")/2,N
      TEMP=0.0d0
      DO 230 J=((1+2)+((1+3)/4)+f((3+2),k=(4+sum(A(:,:,:,4))))),N
  230 TEMP=TEMP+W(J)*SIMI(J,I)
      IF (K .EQ. MP) TEMP=-TEMP
  240 A(I,K)=TEMP
      END
