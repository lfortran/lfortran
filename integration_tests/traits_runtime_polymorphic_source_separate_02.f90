! The contract, a consumer of it with generics taking trait views, and a
! provider with an abstract type that adopts the trait leaving a method
! deferred are compiled separately, in that order. Generic resolution here
! accepts class(RhsBase) and class(Decay) actuals for the view dummies.
program traits_runtime_polymorphic_source_separate_02
    use traits_runtime_polymorphic_source_separate_02_consumer_m, only: Problem, &
        stiff, retune, Integrator
    use traits_runtime_polymorphic_source_separate_02_provider_m, only: RhsBase, Decay
    implicit none
    class(RhsBase), allocatable :: d
    class(Decay), allocatable :: e
    type(Problem) :: p
    type(Integrator) :: integ
    real(8) :: y(2)
    allocate(Decay :: d)
    allocate(e)
    d%k = 2
    e%k = 3
    if (stiff(d) /= 20) error stop 1
    if (stiff(e) /= 30) error stop 2
    call retune(d, 4d0)
    if (d%k /= 4 .or. stiff(d) /= 40) error stop 3
    p = Problem(d, 0.1d0)
    if (abs(p%h - 0.0125d0) > 1d-15) error stop 4
    if (p%f%stiffness() /= 40) error stop 5
    y = [1d0, 2d0]
    call integ%advance(d, y)
    if (any(abs(y - [0.6d0, 1.2d0]) > 1d-14)) error stop 6
    y = [1d0, 2d0]
    call integ%advance(e, y)
    if (any(abs(y - [0.7d0, 1.4d0]) > 1d-14)) error stop 7
    print '(a)', 'traits_runtime_polymorphic_source_separate_02 ok'
end program
