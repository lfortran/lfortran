module a_shadow_same_symbol
    use, intrinsic :: ISO_FORTRAN_ENV, only: int64
    implicit none
end module

module b_shadow_same_symbol
    use a_shadow_same_symbol
    use, intrinsic :: ISO_FORTRAN_ENV, only: int64
    implicit none
end module

program shadow_same_symbol
    use b_shadow_same_symbol
    implicit none
end program