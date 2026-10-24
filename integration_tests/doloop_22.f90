      program doloop_22
      integer :: i, j, s
!     (a) Nested legacy DO loops sharing a terminal action statement.
      s = 0
      do 1 j = 1, 3
      do 1 i = 1, 4
1     s = s + 1
      if (s /= 12) error stop

!     (b) Legacy DO loop ending on a labelled action statement;
!     `go to 2` branches to that statement.
      s = 0
      do 2 i = 1, 5
          if (i == 3) go to 2
          s = s + 10
2     s = s + i
      if (s /= 55) error stop

!     (c) Nested legacy DO loops sharing a terminal `continue`.
      s = 0
      do 3 j = 1, 2
      do 3 i = 1, 5
          if (i == 3) go to 3
          s = s + i
3     continue
      if (s /= 24) error stop

      end program
