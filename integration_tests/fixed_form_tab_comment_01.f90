      program fixed_form_tab_comment_01
      integer i, s
      s = 0
      do i = 1, 3
	    s = s + i
	    !print *, i
      enddo
      do i = 1, 3
	!s = s + 100
      s = s + i
      enddo
      if (s /= 12) error stop 1
	    ! comment after the loop
      end
