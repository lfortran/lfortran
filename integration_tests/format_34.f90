program format_34
  implicit none
  integer,parameter:: k(3) = [-1,2,3]
    print "(A,3(1X,I0))",'mainline k =', k
  call format_34_subprogram
contains
  subroutine format_34_subprogram
    print "(A,3(1X,I0))",'internal k =', k
  end subroutine format_34_subprogram
end program format_34
