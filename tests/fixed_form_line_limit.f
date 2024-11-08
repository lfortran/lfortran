c        1         2         3         4         5         6         7 
c     789012345678901234567890123456789012345678901234567890123456789012
      program test                                                      <-
      int                                                               <-
     $eger a                                                            <-
      print *, "this is a test to see if the fixed form parser properly " <- that quote mark is in c73
     $ignores text past column 72"
      print *, "test that we can still see the quote in the last column"<-
      print *, "test that we avoid double "" lookahead in col 73       ""
      a=                                                                &
     &     5                                                            &
     &     + 3
      end program
