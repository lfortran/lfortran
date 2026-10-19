      program fixed_form_cycle_01
      integer i, j, n, m, k, p
      n = 0
      do i = 1, 3
         if (i == 2) go to 50
         n = n + i
         cycle
   50    n = n + 10*i
      end do
      if (n /= 24) error stop
      m = 0
      do 10 i = 1, 3
         if (i == 2) go to 60
         m = m + i
         cycle
   60    m = m + 10*i
   10 continue
      if (m /= 24) error stop
      k = 0
      do i = 1, 4
         if (mod(i, 2) == 0) go to 20
         k = k + i
         go to 30
   20    cycle
   30    k = k + 100
      end do
      if (k /= 204) error stop
      p = 0
      outer: do i = 1, 3
         do j = 1, 3
            if (j == 2) go to 70
            p = p + 1
            cycle outer
   70       p = p + 100
         end do
      end do outer
      if (p /= 3) error stop
      print *, n, m, k, p
      end
