module traits_runtime_generic_03_ops_m
    use traits_runtime_generic_01_contracts_m, only: IValue
    implicit none
contains
    function transform{IValue :: Q}(object) result(r)
        type(Q), intent(in) :: object
        integer :: r
        r = helper{Q}(object) + 10
    end function
    function helper{IValue :: U}(object) result(r)
        type(U), intent(in) :: object
        integer :: r
        r = object%value()
    end function
end module
