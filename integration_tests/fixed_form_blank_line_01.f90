c     This is a fixed-form test exercising blank lines containing
c     only whitespace (tabs and spaces), including at end of file.
      program fixed_form_blank_line
      call test()
      end program

      subroutine test()
	
      integer :: n
      parameter ( n=10 )
      if (n .ne. 10) error stop 1
      return
      end
