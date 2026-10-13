! The initializers that a separately compiled module bound to MyType without
! traits implement the initializer requirements a later module acknowledges:
! concrete MyType(args) and generic T(args) select the same private
! initializer, and the acknowledged method dispatches through a view. The
! same holds for Iface, whose initializers are the private procedures of its
! standard constructor interface.
program traits_initializers_05
    use traits_initializers_05_contracts, only: from_int, from_real, valued
    use traits_initializers_05_basic, only: MyType
    use traits_initializers_05_enhanced
    use traits_initializers_05_iface, only: Iface
    use traits_initializers_05_iface_enhanced
    implicit none
    type(MyType) :: x
    type(Iface) :: y

    x = MyType(3)
    if (abs(x%value() - 3.0) > 1e-6) error stop 1
    x = from_int{MyType}(3)
    if (abs(x%value() - 3.0) > 1e-6) error stop 2
    x = MyType(2.5)
    if (abs(x%value() - 25.0) > 1e-6) error stop 3
    x = from_real{MyType}(2.5)
    if (abs(x%value() - 25.0) > 1e-6) error stop 4
    if (abs(valued(x) - 25.0) > 1e-6) error stop 5

    y = Iface(3)
    if (abs(y%value() - 3.5) > 1e-6) error stop 6
    y = from_int{Iface}(3)
    if (abs(y%value() - 3.5) > 1e-6) error stop 7
    y = Iface(2.5)
    if (abs(y%value() - 5.0) > 1e-6) error stop 8
    y = from_real{Iface}(2.5)
    if (abs(y%value() - 5.0) > 1e-6) error stop 9
    if (abs(valued(y) - 5.0) > 1e-6) error stop 10
    if (Iface('abcd') /= 4) error stop 11
    print '(a)', 'traits_initializers_05 ok'
end program traits_initializers_05
