subroutine sub2(N,JA)
    implicit none
    integer,intent(in) :: N, ja(N)
    print "(*(1X,I0))",ja
end subroutine sub2

program format_26
    call sub(2,[42,666])
    call sub2(2,[42,666])
contains

subroutine sub(N,JA)
    implicit none
    integer,intent(in) :: N, ja(:)
    print "(*(1X,I0))",ja
end subroutine sub

end program format_26