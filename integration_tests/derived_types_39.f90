module derived_types_39_mod

    type :: tt
       integer :: ll
    contains
       procedure :: ssub 
       procedure :: ff 
    end type tt
 
 contains
 
    subroutine ssub(dt, checker)
       class(tt), intent(in) :: dt
       integer :: checker
       print *, checker , " -- ", dt%ll
       if(checker /= dt%ll) error stop
    end subroutine ssub
 
 
    function ff(dt) result(i)
       class(tt), intent(in) :: dt
       integer :: i
       i = dt%ll
       print *, i
    end function ff
 
 end module derived_types_39_mod
 
 
 program derived_types_39
 
    use derived_types_39_mod
    type(tt) :: vals2(3)
 
 
    vals2(1)%ll = 11
    vals2(2)%ll = 22
    vals2(3)%ll = 33
 
    ! Check derivedType subroutine call
    call vals2(1)%ssub(11)
    call vals2(2)%ssub(22)
    call vals2(3)%ssub(33)
    
    ! Check derivedType function call
    if(vals2(1)%ff() /= 11) error stop
    if(vals2(2)%ff() /= 22) error stop
    if(vals2(3)%ff() /= 33) error stop
 
 
 end program derived_types_39