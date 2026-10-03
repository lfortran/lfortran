      program fixed_form_stmt_func_01
      implicit none
      integer x, y
      integer ifos10, add2
      ifos10() = 42
      add2(x) = x + 2
      y = ifos10()
      if (y .ne. 42) stop 1
      y = add2(5)
      if (y .ne. 7) stop 2
      print *, 'PASSED'
      end
