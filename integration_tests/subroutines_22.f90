module mr
implicit none

type :: tt
    real, allocatable :: v(:)
end type tt

type :: wrapper_t
    type(tt), pointer :: ptr
end type wrapper_t

contains

    pure function f(a) result(r)
        real, intent(in) :: a(:)
        type(tt) :: r
        r%v = a
    end function f

    subroutine assoc(p, t)
        type(tt), pointer, intent(out) :: p
        type(tt), target, intent(in) :: t
        p => t
    end subroutine assoc

end module mr

program subroutines_22
use mr
implicit none

call branch_case()
call callee_case()
call loop_case()
call component_case()

contains

    subroutine branch_case()
        type(tt), target :: w
        type(tt), pointer :: p
        logical :: cond

        allocate(w%v(3))
        w%v = [1.0, 2.0, 3.0]
        cond = .true.
        if (cond) then
            p => w
        else
            p => null()
        end if
        w = f(p%v)
        print *, w%v
        if (any(w%v /= [1.0, 2.0, 3.0])) error stop 1
    end subroutine branch_case

    subroutine callee_case()
        type(tt), target :: w
        type(tt), pointer :: p

        allocate(w%v(3))
        w%v = [1.0, 2.0, 3.0]
        call assoc(p, w)
        w = f(p%v)
        print *, w%v
        if (any(w%v /= [1.0, 2.0, 3.0])) error stop 2
    end subroutine callee_case

    subroutine loop_case()
        type(tt), target :: t1, t2
        type(tt), pointer :: pp, qq
        integer :: i

        allocate(t1%v(3))
        t1%v = [1.0, 2.0, 3.0]
        allocate(t2%v(3))
        t2%v = [7.0, 8.0, 9.0]
        qq => t2
        do i = 1, 2
            pp => qq
            qq => t1
            if (i == 2) then
                t1 = f(pp%v)
            end if
        end do
        print *, t1%v
        if (any(t1%v /= [1.0, 2.0, 3.0])) error stop 3
    end subroutine loop_case

    subroutine component_case()
        type(wrapper_t) :: w_obj
        type(tt), target :: t_obj

        allocate(t_obj%v(3))
        t_obj%v = [1.0, 2.0, 3.0]
        w_obj%ptr => t_obj
        t_obj = f(w_obj%ptr%v)
        print *, t_obj%v
        if (any(t_obj%v /= [1.0, 2.0, 3.0])) error stop 4
    end subroutine component_case

end program subroutines_22