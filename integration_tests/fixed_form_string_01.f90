      program fixed_form_string_01
      implicit none
      character(len=80) :: s, t, u
c     A character literal reaching column 72, continued on a regular
c     continuation line
      s = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     +bcdefg"
c     The same literal continued on a tab-format continuation line
      t = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
	1bcdefg"
c     The same literal continued after a comment line and a blank line
      u = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
c     comment inside the continued literal

     +bcdefg"
      print *, len_trim(s), len_trim(t)
      print *, s(60:)
      print *, t(60:)
      print *, u(60:)
      if (len_trim(s) /= 67) error stop
      if (s(60:) /= "aabcdefg") error stop
      if (t /= s) error stop
      if (u /= s) error stop
      end program
