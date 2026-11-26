! Module variables with the SAVE attribute written out are freed when the
! program ends, as those without it are, under --detect-leaks (issue #17934):
! a module variable is saved whether the attribute is written or not.
module detect_leaks_02_m
    implicit none
    type :: box
        integer, allocatable :: v(:)
        character(len=:), allocatable :: name
    end type
    real, allocatable, save :: g(:)
    real, allocatable :: h(:)
    type(box), save :: b
    type(box), allocatable, save :: bs(:)
    character(len=:), allocatable, save :: label
contains
    subroutine fill(n)
        integer, intent(in) :: n
        integer :: i
        allocate(g(n))
        g = [(real(i), i = 1, n)]
        allocate(h(2*n))
        h = 1
        allocate(b%v(n))
        b%v = 7
        b%name = "first"
        allocate(bs(2))
        allocate(bs(2)%v(3))
        bs(2)%v = [1, 2, 3]
        label = "saved"
    end subroutine
end module

program detect_leaks_02
    use detect_leaks_02_m
    implicit none
    call fill(5)
    if (size(g) /= 5 .or. abs(sum(g) - 15) > 1e-6) error stop 1
    if (size(h) /= 10) error stop 2
    if (any(b%v /= 7) .or. b%name /= "first") error stop 3
    if (allocated(bs(1)%v) .or. sum(bs(2)%v) /= 6) error stop 4
    if (label /= "saved") error stop 5
    print *, "ok"
end program
