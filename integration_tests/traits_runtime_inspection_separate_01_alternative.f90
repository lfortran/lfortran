subroutine inspection_alternative(view, n)
    use traits_runtime_inspection_contracts_m, only: IRich, PublicCell
    use traits_runtime_inspection_alternative_m
    implicit none
    class(IRich), pointer, intent(in) :: view
    integer, intent(in) :: n
    class(IRich), pointer :: repacked
    select type (concrete => view)
    type is (PublicCell)
        if (concrete%n /= n .or. view%value() /= n .or. view%label() /= 101) error stop 31
        repacked => concrete
    class default
        error stop 32
    end select
    if (repacked%value() /= n + 1000 .or. repacked%label() /= 303) error stop 33
    select type (concrete => repacked)
    type is (PublicCell)
        if (concrete%n /= n) error stop 34
    class default
        error stop 35
    end select
    if (view%value() /= n .or. view%label() /= 101) error stop 36
    nullify(repacked)
end subroutine
