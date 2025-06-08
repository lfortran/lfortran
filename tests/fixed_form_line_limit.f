c        1         2         3         4         5         6         7 
c     789012345678901234567890123456789012345678901234567890123456789012
      program test                                                      <-
      int                                                               <-
     $eger a                                                            <-
      print *, "this is a test to see if the fixed form parser properly " <- that quote mark is in c73
     $ignores text past column 72"
      print *, "test that we can still see the quote in the last column"<-
      print *, "test that we avoid double "" lookahead in col 73       ""
      print *, "                                  end-of-line double """"
c The next line only has text past column 72
                                                                        00770402
c     The next pattern is used to do continuations in code that is read
c     by both fixed-form AND free-form parsers
      a=                                                                &
     &     5                                                            &
     &     + 3
      end program
