! A procedure pointer component whose interface has an assumed-length
! character dummy keeps the hidden length argument when the module is read
! from its .mod file.
module separate_compilation_59a
implicit none
abstract interface
    subroutine f_i(name, n)
        character(len=*), intent(in) :: name
        integer, intent(out) :: n
    end subroutine
end interface
type :: t
    procedure(f_i), pointer, nopass :: f => null()
end type
contains
subroutine reg(x, fp)
    type(t), intent(inout) :: x
    procedure(f_i) :: fp
    x%f => fp
end subroutine
end module
