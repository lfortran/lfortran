module lp_union_test_01_mod
    implicit none
    
    _lfortran_union_type :: test_type4 
        integer :: x
        real    :: i
    end _lfortran_union_type
end module


program lp_union_test_01
    use lp_union_test_01_mod
    implicit none 
    type:: test_type
        integer :: x
        real    :: y
    end type
    
    _lfortran_union_type :: test_type1 ! this is a test comment
        integer :: x
        real    :: i
        type(test_type) :: y
    end _lfortran_union_type
    
    
    _lfortran_union_type :: test_type2 
        integer :: x
        real    :: i
        type(test_type1) :: y
    end _lfortran_union_type
    
    type :: test_type3 
        integer :: x
        real    :: i
        type(test_type) :: y
        type(test_type1) :: z
        type(test_type2) :: w
    end type
    
    type(test_type) :: t_ty
    type(test_type1) :: t_ty1
    type(test_type2) :: t_ty2
    type(test_type3) :: t_ty3
    type(test_type4) :: t_ty4

end program

