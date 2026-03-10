module modules_70_constants
    implicit none
    public
    real, parameter :: alpha = 1.0
    real, parameter :: beta = 2.0
end module modules_70_constants

module modules_70_sparse_constants
    use modules_70_constants
    implicit none
    public
    integer, parameter :: ilp = selected_int_kind(9)
end module modules_70_sparse_constants

module modules_70_sparse_kinds
    use modules_70_sparse_constants
    implicit none
    private
    public :: ilp
end module modules_70_sparse_kinds

module modules_70_sparse
    use modules_70_sparse_kinds
    implicit none
end module modules_70_sparse

program modules_70
    use modules_70_sparse
    implicit none
    real :: alpha
    alpha = 3.0
    if (abs(alpha - 3.0) > 1e-6) error stop
    print *, "PASS"
end program modules_70
