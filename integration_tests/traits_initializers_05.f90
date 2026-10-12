! The initializers that a separately compiled module bound to MyType without
! traits implement the initializer requirements a later module acknowledges:
! concrete MyType(args) and generic T(args) select the same private
! initializer, and the acknowledged method dispatches through a view.
program traits_initializers_05
    use traits_initializers_05_contracts, only: from_int, from_real, valued
    use traits_initializers_05_basic, only: MyType
    use traits_initializers_05_enhanced
    implicit none
    type(MyType) :: x

    x = MyType(3)
    if (abs(x%value() - 3.0) > 1e-6) error stop 1
    x = from_int{MyType}(3)
    if (abs(x%value() - 3.0) > 1e-6) error stop 2
    x = MyType(2.5)
    if (abs(x%value() - 25.0) > 1e-6) error stop 3
    x = from_real{MyType}(2.5)
    if (abs(x%value() - 25.0) > 1e-6) error stop 4
    if (abs(valued(x) - 25.0) > 1e-6) error stop 5
    print '(a)', 'traits_initializers_05 ok'
end program traits_initializers_05
