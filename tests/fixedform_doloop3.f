      SUBROUTINE START1(N, F, L, LOUT)
C
C       ALGORITHM AS 93.1 APPL. STATIST. (1976) VOL.25, NO.1
C
C       GENERATES A 1,N ANSARI-BRADLEY DISTRIBUTION IN F.
C
      REAL F(L), ONE=1, TWO=2
      LOUT = 1 + N / 2
      DO 1 I = 1, LOUT
1     F(I) = TWO
      IF (MOD(N, 2) .EQ. 0) F(LOUT) = ONE
      RETURN
      END

      SUBROUTINE f(N)
      DO 1 I = 1, LOUT
        a = 1
        do 3 j = 3, n
            b = 2
            do 4 k = 3, n
                c = 3
                do 5 l = 3, n
                    d = 4
5               xx = 5
                e = 5
4           end do
            f = 6
        end do
        g = 8
1     F(I) = TWO
      h = 9
      END
