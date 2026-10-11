! Module variables associated by EQUIVALENCE. The equivalenced ones are laid
! out as aliases of the storage they share, in this translation unit only.
module equivalence_49_module
    implicit none
    real :: matrix(2,2), flat(4)
    equivalence (matrix, flat)
    integer :: third, items(5)
    equivalence (third, items(3))
end module
