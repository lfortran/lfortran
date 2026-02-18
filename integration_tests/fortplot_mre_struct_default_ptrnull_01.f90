module fortplot_mre_struct_default_ptrnull_mod
    use iso_c_binding, only: c_ptr, c_null_ptr
    implicit none

    type :: boxed_ptr
        type(c_ptr) :: p = c_null_ptr
    end type boxed_ptr

    type(boxed_ptr), save :: x
end module fortplot_mre_struct_default_ptrnull_mod

program fortplot_mre_struct_default_ptrnull_01
    use iso_c_binding, only: c_associated
    use fortplot_mre_struct_default_ptrnull_mod, only: x
    implicit none

    write(*, '(L1)') c_associated(x%p)
end program fortplot_mre_struct_default_ptrnull_01
