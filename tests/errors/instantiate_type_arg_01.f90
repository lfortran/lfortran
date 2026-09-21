! An INSTANTIATE statement whose argument for a deferred type names something
! that is not declared. C1627 requires such an argument to specify an intrinsic
! type or a previously defined nonintrinsic type; `no_such_type` is neither.
! This used to dereference the unresolved symbol and segfault.

module instantiate_type_arg_01_mod
    implicit none

    template tmpl(t)
        deferred type :: t
    contains
        subroutine s(x)
            type(t), intent(in) :: x
        end subroutine
    end template

end module instantiate_type_arg_01_mod

program instantiate_type_arg_01
    use instantiate_type_arg_01_mod
    implicit none

    instantiate tmpl {no_such_type}, only: s
end program instantiate_type_arg_01
