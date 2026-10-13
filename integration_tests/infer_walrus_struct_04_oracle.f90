! Standard-Fortran oracle of infer_walrus_struct_04, with the declarations
! that `:=` infers written out.
module infer_walrus_struct_04_oracle_m
    implicit none
    private
    public :: point, tagged, tag_of

    type :: point
        integer :: x = 0
    end type point

    type :: tagged
        private
        integer :: n = 0
    end type tagged

    interface tagged
        module procedure make_tagged
    end interface tagged

contains

    function make_tagged(n) result(res)
        integer, intent(in) :: n
        type(tagged)        :: res
        res%n = 10 * n
    end function make_tagged

    integer function tag_of(t)
        type(tagged), intent(in) :: t
        tag_of = t%n
    end function tag_of
end module infer_walrus_struct_04_oracle_m

program infer_walrus_struct_04_oracle
    use infer_walrus_struct_04_oracle_m, only: point, tagged, tag_of
    implicit none
    type(point)  :: a(3), b(4)
    type(tagged) :: c(3)
    integer      :: i
    a = [point(1), point(2), point(3)]
    b = [(point(i), i = 1, 4)]
    c = [(tagged(i), i = 1, 3)]
    if (size(a) /= 3 .or. a(3)%x /= 3) error stop 1
    if (size(b) /= 4 .or. b(4)%x /= 4 .or. sum(b%x) /= 10) error stop 2
    if (size(c) /= 3 .or. tag_of(c(2)) /= 20 .or. tag_of(c(3)) /= 30) error stop 3
    call check(b)
    print *, "PASSED"
contains
    subroutine check(points)
        type(point), intent(in) :: points(:)
        type(point) :: d(4)
        integer     :: j
        d = [(point(2 * j), j = 1, 4)]
        if (any(d%x /= 2 * points%x)) error stop 4
    end subroutine check
end program infer_walrus_struct_04_oracle
