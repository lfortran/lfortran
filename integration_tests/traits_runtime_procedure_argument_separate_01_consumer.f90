! The consumer, compiled without the provider: it sets up a problem in a
! container through views and an owner component, registers a factory of its
! own and resolves the registrations.
module traits_procarg_sep_01_consumer_m
    use traits_procarg_sep_01_contracts_m, only: IResolver, &
        IContainer, IProblem, ifactory, Mesh
    implicit none
    type :: Application
        class(IContainer), allocatable :: c
    end type Application
contains
    subroutine setup(p, c)
        class(IProblem), intent(in) :: p
        class(IContainer), intent(inout) :: c
        procedure(ifactory), pointer :: f
        call p%registration(c)
        f => answer
        call c%register("answer", f)
    end subroutine setup

    subroutine setup_application(app, p)
        type(Application), intent(inout) :: app
        class(IProblem), intent(in) :: p
        call setup(p, app%c)
    end subroutine setup_application

    function answer(c) result(res)
        class(IResolver), intent(in) :: c
        class(*), allocatable :: res
        class(*), allocatable :: m
        m = c%resolve("mesh")
        select type (m)
        type is (Mesh)
            res = m%cells + 1
        class default
            res = 0
        end select
    end function answer

    subroutine check_all(c)
        class(IResolver), intent(in) :: c
        class(*), allocatable :: r
        r = c%resolve("count")
        select type (r)
        type is (integer)
            if (r /= 42) error stop 10
        class default
            error stop 11
        end select
        r = c%resolve("width")
        select type (r)
        type is (real(8))
            if (abs(r - 2.5d0) > 1d-12) error stop 12
        class default
            error stop 13
        end select
        r = c%resolve("name")
        select type (r)
        type is (character(*))
            if (r /= "heat equation") error stop 14
        class default
            error stop 15
        end select
        r = c%resolve("mesh")
        select type (r)
        type is (Mesh)
            if (r%cells /= 42 .or. abs(r%width - 2.5d0) > 1d-12) error stop 16
        class default
            error stop 17
        end select
        r = c%resolve("answer")
        select type (r)
        type is (integer)
            if (r /= 43) error stop 18
        class default
            error stop 19
        end select
        r = c%resolve("unknown")
        select type (r)
        type is (integer)
            if (r /= -1) error stop 20
        class default
            error stop 21
        end select
    end subroutine check_all
end module traits_procarg_sep_01_consumer_m
