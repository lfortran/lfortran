module julienne_string_m_multiple_procs_in_bin_op
    use iso_c_binding, only : c_bool
    implicit none
    private
    public :: string_t
    public :: operator(.separatedBy.)
  
    type string_t
      private
      character(len=:), allocatable :: string_
    end type
  
    interface operator(.csv.)
  
      pure module function strings_with_comma_separator(strings) result(csv)
        implicit none
        type(string_t), intent(in) :: strings(:)
        type(string_t) csv
      end function
  
    end interface
  
    interface operator(.separatedBy.)

      pure module function strings_with_separator(strings, sep) result(joined)
        implicit none
        type(string_t), intent(in) :: strings(:)
        character(len=*), intent(in) :: sep
        type(string_t) :: joined
      end function
      
    end interface
  
  contains
  
  
    module procedure strings_with_comma_separator
    end procedure
  
    module procedure strings_with_separator
    end procedure
  
  end module julienne_string_m_multiple_procs_in_bin_op
  
  
  program create_markdown_table
  
    use julienne_string_m_multiple_procs_in_bin_op
    implicit none
  
  contains
  
    pure function markdown_table(column_header) result(lines)
      type(string_t), intent(in) :: column_header(:)
      character(len=1), parameter :: column_separator = "|"
      type(string_t) lines(5)
  
      lines(1) = column_header .separatedBy. column_separator
  
    end function
  
  end program