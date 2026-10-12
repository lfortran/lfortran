! A client compiled after its provider (traits_protocol_static_05_provider)
! calls the static-only conformance statically and the other conformance of
! the same module through its runtime view.
program traits_protocol_static_05
    use traits_protocol_static_05_contracts_m, only: ISum
    use traits_protocol_static_05_intrinsics_m
    use traits_protocol_static_05_provider_m, only: Vec, Rev, vec_sum
    implicit none
    class(ISum), allocatable :: v
    type(Vec) :: g
    integer :: iv(4), k

    iv = [1, 20, 300, 4000]
    k = vec_sum(iv)
    print '(a,i0)', 'static vec_sum(x([1,3])) = ', k
    if (k /= 301) error stop 1
    if (g%sum(iv) /= 301) error stop 2
    v = Rev()
    k = v%sum(iv)
    print '(a,i0)', 'runtime rev sum = ', k
    if (k /= 4321) error stop 3
    print '(a)', 'traits_protocol_static_05 ok'
end program traits_protocol_static_05
