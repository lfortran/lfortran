program lp_union_test_01
    
    type:: test_type
        integer :: x
        real    :: y
    end type
    
    _lfortran_union_type :: test_type1 ! this is a test comment
        integer :: x
        real    :: i
        type(test_type) :: y
    end _lfortran_union_type
    
    
    _lfortran_union_type :: test_type2 ! this is a test comment
        integer :: x
        real    :: i
        type(test_type1) :: y
    end _lfortran_union_type
    
    type :: test_type3 ! this is a test comment
        integer :: x
        real    :: i
        type(test_type) :: y
        type(test_type1) :: z
        type(test_type2) :: w
    end type
    
    type(test_type) :: t_ty
    type(test_type1) :: t_ty1
    type(test_type2) :: t_ty2

end program

