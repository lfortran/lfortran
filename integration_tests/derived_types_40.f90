module derived_types_40_func_type
abstract interface
    subroutine OBJ(x)
    real, intent(inout) :: x(:)
    end subroutine 
end interface
contains
subroutine temp_calfun(x)
    real, intent(inout) :: x(:)
    x = x + 1
end subroutine temp_calfun
end module derived_types_40_func_type

module derived_types_40_prob_mod
 use derived_types_40_func_type
 public :: temp_calfun
 type PROB_T
    procedure(OBJ),nopass, pointer :: calfun => null()
    procedure(OBJ),nopass, pointer :: calfun2 => temp_calfun
 end type PROB_T
contains 

subroutine construct(prob)
    type(PROB_T), intent(out) :: prob
    real :: x(5)
    x = 0
    prob % calfun => temp_calfun
    call prob % calfun(x)
    if (any(x /= 1.0)) error stop
    call prob % calfun2(x)
    if (any(x /= 2.0)) error stop
end subroutine 
end module

program derived_types_40
    use derived_types_40_prob_mod
    type(PROB_T) :: prob
    call construct(prob)
end program