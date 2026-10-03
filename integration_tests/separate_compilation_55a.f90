! A module procedure with a binding label whose internal procedure reads a
! local of it by host association. Compiled separately, the object file of
! this module alone defines the storage that shares that local with the
! internal procedure; see separate_compilation_55.f90.
module separate_compilation_55_m
    use iso_c_binding, only: c_int
    implicit none
contains
    subroutine p(x) bind(c, name="separate_compilation_55_p")
        integer(c_int), intent(inout) :: x
        integer(c_int) :: work(3)
        work = x
        x = helper()
    contains
        integer(c_int) function helper()
            helper = sum(work) + size(work)
        end function helper
    end subroutine p
end module separate_compilation_55_m
