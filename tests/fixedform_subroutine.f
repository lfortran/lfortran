      subroutine f(a,b,c)
          print *,4
      end subroutine
        integer k,l
        k=100
      do 80 k=1,10
        print*,k
   80 continue

      if (k.le.10000) then
          print*,k
      else
          print*,k+1
          do l=1,10
                print*,"do"
                enddo
      endif
      end

