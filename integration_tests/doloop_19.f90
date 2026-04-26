      program doloop_19
      integer :: i, s
!     (a) plain DO loop: no label on `do`, no label on `end do`.
      s = 0
      do i = 1, 5
          s = s + i
      end do
      if (s /= 15) error stop

!     (b) Label only on `end do`. `go to 1` branches to the loop
!     terminator, which cycles the iteration (F2018 11.1.7.5).
      s = 0
      do i = 1, 5
          if (i == 3) go to 1
          s = s + i
1     end do
      if (s /= 12) error stop

!     (c) Labels on both `do` and `end do` (modern labelled DO).
      s = 0
      do 2 i = 1, 5
          if (i == 3) go to 2
          s = s + i
2     end do
      if (s /= 12) error stop

!     (d) Legacy `DO <label> ... <label> CONTINUE` form.
      s = 0
      do 3 i = 1, 5
          if (i == 3) go to 3
          s = s + i
3     continue
      if (s /= 12) error stop

      end program
