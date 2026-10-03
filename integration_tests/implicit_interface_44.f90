      program implicit_interface_44
      double precision diff, x, y, z
      x = 3.0d0
      y = 1.0d0
      z = diff(x, y)
      if (z .ne. 2.0d0) error stop
      end

      double precision function diff(x, y)
      double precision x, y
      diff = x - y
      end
