module allocatable_polymorphic_assign_03_mod
    implicit none
    type :: sample_t
        integer :: val = 0
    end type sample_t
contains
    function make_struct(n) result(r)
        integer, intent(in) :: n
        type(sample_t) :: r(n, 1)
        integer :: i
        do i = 1, n
            r(i, 1)%val = i * 10
        end do
    end function make_struct

    subroutine do_assign(n)
        integer, intent(in) :: n
        class(*), allocatable :: a(:,:)
        a = make_struct(n)
        if (allocated(a)) deallocate(a)
    end subroutine do_assign
end module allocatable_polymorphic_assign_03_mod

program allocatable_polymorphic_assign_03
    use allocatable_polymorphic_assign_03_mod, only: do_assign
    implicit none
    integer :: dummy
    dummy = 0
    if (dummy /= 0) call do_assign(3)
end program allocatable_polymorphic_assign_03
