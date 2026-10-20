! Standard-Fortran counterpart of traits_runtime_procedure_argument_separate_01.f90.
program traits_runtime_procedure_argument_separate_01_oracle
    use traits_procarg_sep_01_oracle_contracts_m, only: IContainer, IProblem
    use traits_procarg_sep_01_oracle_provider_m, only: Container, Problem
    use traits_procarg_sep_01_oracle_consumer_m, only: Application, &
        setup, setup_application, check_all
    implicit none
    class(IContainer), allocatable :: c
    class(IProblem), allocatable :: p
    class(IContainer), pointer :: cp
    type(Container), target :: direct
    type(Application) :: app

    allocate(Container :: c)
    allocate(Problem :: p)
    call setup(p, c)
    call check_all(c)

    cp => direct
    call setup(p, cp)
    call check_all(cp)
    if (direct%n /= 5 .or. direct%missing /= 1) error stop 30

    allocate(Container :: app%c)
    call setup_application(app, Problem(3))
    call check_all(app%c)
    select type (x => app%c)
    type is (Container)
        if (x%n /= 5 .or. x%missing /= 1) error stop 31
    class default
        error stop 32
    end select
    print *, "ok"
end program traits_runtime_procedure_argument_separate_01_oracle
