      program while_03
      implicit none
      integer :: n, i, j

      i = 0
      n = 2

      do while ( n >= 2 )
         if( n >= i ) then
            if( n >= i ) then
               do j= i+1,n
                  i = j + i
               end do
            endif
            n = n - 1
         else
            i = j + i
         endif
      end do

      return
      end

