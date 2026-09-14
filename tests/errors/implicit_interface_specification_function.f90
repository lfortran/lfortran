! A procedure with an implicit interface cannot be known to be pure, so it is
! not a specification function and cannot be referenced in a specification
! expression (F2018 10.1.11).
subroutine implicit_interface_specification_function(n)
    implicit none
    integer, intent(in) :: n
    integer, external :: work_size
    real :: work(work_size(n))
    work = 0
end subroutine
