c     Labeled DO loops whose terminal statement carries the label, or
c     whose label is shared by nested loops: `fmt`/`--show-ast-f90` must
c     print each label exactly once (lfortran/lfortran#15952).
      program fixed_form_do_label1
      integer :: i, j, k, it
      it = 0
c     nested loops sharing a label on an action statement
      do 11 j = 1, 10
      do 11 i = 1, 10
11    it = it + 1
c     three nested loops sharing a label on an action statement
      do 12 k = 1, 2
      do 12 j = 1, 2
      do 12 i = 1, 2
12    it = it + 1
c     single loop terminated by an action statement
      do 13 i = 1, 4
13    it = it + 10
c     nested loops sharing a CONTINUE terminal
      do 14 j = 1, 2
      do 14 i = 1, 5
      it = it + 100
14    continue
c     branch to a shared CONTINUE terminal cycles the innermost loop
      do 15 j = 1, 3
      do 15 i = 1, 4
      if (i == 2) goto 15
      it = it + 1000
15    continue
c     branch to a shared action-statement terminal
      do 16 j = 1, 2
      do 16 i = 1, 3
      if (i == 1) go to 16
      it = it + 10000
16    it = it + 1
c     consecutive loops, each with its own action-statement terminal
      do 17 i = 1, 2
17    it = it + 100000
      do 18 i = 1, 3
18    it = it + 100000
c     non-shared CONTINUE terminals
      do 20 j = 1, 2
      do 19 i = 1, 2
      it = it + 1000000
19    continue
20    continue
      print *, it
      if (it /= 4550154) error stop
      end program
