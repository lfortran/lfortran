! A procedure with an implicit interface cannot be known to be pure, so it is
! not a specification function and cannot be referenced in a specification
! expression (F2018 10.1.11), whether it is an external or a dummy procedure.
subroutine implicit_interface_specification_function(n)
    implicit none
    integer, intent(in) :: n
    integer, external :: work_size
    real :: work(work_size(n))
end subroutine

subroutine implicit_interface_specification_function_dummy(f, n)
    implicit none
    integer, intent(in) :: n
    integer, external :: f
    real :: work(f(n))
end subroutine
