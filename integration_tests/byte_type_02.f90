! Test the non-standard `byte` type in fixed-form.
      program byte_type_02
      implicit none
      byte b
      byte c(3)
      integer i
      b = 5
      if (kind(b) .ne. 1) error stop
      if (b .ne. 5) error stop
      do i = 1, 3
          c(i) = int(i, kind=1)
      end do
      if (c(2) .ne. 2) error stop
      print *, b, c(2)
      end program
