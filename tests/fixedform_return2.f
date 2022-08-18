      subroutine f(a,b,c)
      print *, a
      go to 1
      return
    1  print *, b
      return
      end subroutine

      function g(a,b,c)
      print *, a
      go to 2
      return
    2  print *, b
      return
      end function
