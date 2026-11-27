! CONTAINS in an inactive preprocessor branch is not a source statement.
program preprocessor25
  interface
    subroutine q()
      integer :: n
#if 0
      contains
#endif
    end subroutine
  end interface
end program preprocessor25
