! An internal function with an allocatable array result can be referenced
! through a dummy procedure or a procedure pointer: the procedure value
! returns the result of the internal function straight into its own result.
module procedure_pointer_43_mod
implicit none
type :: t
    integer :: v = 0
contains
    final :: finalize
end type
abstract interface
    function matrix() result(r)
        real, allocatable :: r(:,:)
    end function
end interface
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    x%v = -1
end subroutine
integer function total(f)
    interface
        function f() result(r)
            integer, allocatable :: r(:)
        end function
    end interface
    total = sum(f())
end function
integer function total_t(f)
    interface
        function f() result(r)
            import :: t
            type(t), allocatable :: r(:)
        end function
    end interface
    type(t), allocatable :: x(:)
    x = f()
    if (size(x) /= 2) error stop
    total_t = sum(x%v)
end function
subroutine host(n)
    integer, intent(in) :: n
    procedure(matrix), pointer :: p
    real, allocatable :: a(:,:)
    if (total(make) /= 3*n) error stop
    p => make_matrix
    a = p()
    if (size(a, 1) /= n .or. size(a, 2) /= n + 1) error stop
    if (abs(sum(a) - 2.0*n*(n + 1)) > 1e-6) error stop
    if (total_t(make_t) /= 2*n) error stop
contains
    function make() result(r)
        integer, allocatable :: r(:)
        allocate(r(3))
        r = n
    end function
    function make_matrix() result(r)
        real, allocatable :: r(:,:)
        allocate(r(n, n + 1))
        r = 2
    end function
    function make_t() result(r)
        type(t), allocatable :: r(:)
        allocate(r(2))
        r%v = n
    end function
end subroutine
end module
program procedure_pointer_43
use procedure_pointer_43_mod
implicit none
call host(3)
print *, "ok"
end program
