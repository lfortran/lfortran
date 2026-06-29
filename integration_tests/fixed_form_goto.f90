      program fixed_form_goto
      implicit none
      integer n
      parameter(n=4)
      double precision a(n,n)
      integer i,j

C Initialize matrix to zero
      do 10 i=1,n
         do 20 j=1,n
            a(i,j)=0.0d0
20       continue
10    continue

C Example companion matrix
      a(2,1)=1.0d0
      a(3,2)=1.0d0
      a(4,3)=1.0d0

      a(1,4)=-10.0d0
      a(2,4)=-20.0d0
      a(3,4)=-30.0d0
      a(4,4)=-40.0d0

      call balance_companion(n,a)

      if (.not. all(abs(a(1,:) - (/ 0.0d0, 0.0d0, 0.0d0,
     &    -0.625d0 /)) < 1.0d-12)) then
         error stop 'Row 1 not balanced correctly'
      endif
      if (.not. all(abs(a(2,:) - (/ 1.0d0, 0.0d0, 0.0d0,
     &    -1.25d0 /)) < 1.0d-12)) then
         error stop 'Row 2 not balanced correctly'
      endif
      if (.not. all(abs(a(3,:) - (/ 0.0d0, 2.0d0, 0.0d0,
     &    -3.75d0 /)) < 1.0d-12)) then
         error stop 'Row 3 not balanced correctly'
      endif
      if (.not. all(abs(a(4,:) - (/ 0.0d0, 0.0d0, 8.0d0,
     &    -40.0d0 /)) < 1.0d-12)) then
         error stop 'Row 4 not balanced correctly'
      endif

      print *, 'PASS'
      end

      subroutine balance_companion(n,a)
      implicit none
      integer, intent(in) :: n
      double precision, intent(inout) :: a(n,n)
      integer, parameter :: b = 2
      integer, parameter :: b2 = b**2
      integer i,j
      double precision c,f,g,r,s
      logical noconv

      if (n <= 1) then
         return
      endif

1     continue
      noconv = .false.

      do 100 i=1,n
         if (i /= n) then
            c = abs(a(i+1,i))
         else
            c = 0.0d0
            do 110 j=1,n-1
               c = c + abs(a(j,n))
110         continue
         endif

         if (i == 1) then
            r = abs(a(1,n))
         elseif (i /= n) then
            r = abs(a(i,i-1)) + abs(a(i,n))
         else
            r = abs(a(i,i-1))
         endif

         if (c == 0.0d0 .or. r == 0.0d0) goto 100

         g = r / b
         f = 1.0d0
         s = c + r

3        if (c < g) then
            f = f * b
            c = c * b2
            goto 3
         endif

         g = r * b
4        if (c >= g) then
            f = f / b
            c = c / b2
            goto 4
         endif

         if ((c + r) / f < 0.95d0 * s) then
            g = 1.0d0 / f
            noconv = .true.

            if (i == 1) then
               a(1,n) = a(1,n) * g
            else
               a(i,i-1) = a(i,i-1) * g
               a(i,n) = a(i,n) * g
            endif

            if (i /= n) then
               a(i+1,i) = a(i+1,i) * f
            else
               do 120 j=1,n
                  a(j,i) = a(j,i) * f
120            continue
            endif
         endif

100   continue

      if (noconv) goto 1
      end subroutine balance_companion
