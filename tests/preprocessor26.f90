! A macro definition that names CONTAINS does not add a CONTAINS statement.
program preprocessor26
  interface
    subroutine q()
      integer :: n
#define MARKER contains
    end subroutine
  end interface
end program preprocessor26
