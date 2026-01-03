      subroutine f()
      use iso_c_binding, only: c_loc, c_f_pointer
      print *, "OK"
      end

      integer function g()
      use iso_c_binding, only: c_loc, c_f_pointer
      print *, "OK"
      g = 5
      end
