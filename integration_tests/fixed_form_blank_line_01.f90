c     This is a fixed-form test
      program fixed_form_blank_line
      call test()
      end program

      subroutine test()
	 
      integer :: n
      parameter ( n=10 )
      if (n .ne. 10) error stop 1
      return
      end
