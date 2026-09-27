! A bind(c) procedure whose allocatable dummy is passed through a C
! descriptor and whose automatic array takes its extent from module state,
! which a declaration initializer gives. The procedure's entry dispatches the
! module's initialization before that extent is evaluated, as global_init_24
! checks from a C constructor; that must not stop such a procedure from
! compiling.
module global_init_28_m
    implicit none
    type :: config
        integer :: count = 4
    end type
    type(config) :: settings(1) = config(4)
contains
    subroutine fill(a) bind(c)
        integer, allocatable, intent(in) :: a(:)
        integer :: work(settings(1)%count)
        if (size(work) /= 4) error stop 1
        work = 1
        if (size(a) /= size(work)) error stop 2
        if (sum(a) /= sum(work)) error stop 3
    end subroutine fill
end module global_init_28_m

program global_init_28
    use global_init_28_m
    implicit none
    integer, allocatable :: a(:)
    allocate(a(4))
    a = 1
    call fill(a)
end program global_init_28
