module intent_02_mod
    type tt
       character(:), allocatable :: ssss
    end type tt
 
    contains
 
    subroutine sub1(str_tt)
       type(tt), intent(out) :: str_tt
       print *,allocated(str_tt%ssss)
       if(allocated(str_tt%ssss) .neqv. .false.) error stop
    end subroutine
 
    subroutine sub2(str)
       character(:), allocatable, intent(out) :: str
       print *, allocated(str)
       if(allocated(str) .neqv. .false.) error stop
    end subroutine
 
 end module
 
 program intent_02 
    use intent_02_mod
    type(tt) :: s1
    character(:), allocatable :: allocatable_str
    allocate(character(10) :: allocatable_str)
    allocate( character(10) :: s1%ssss)
    call sub1(s1)
    call sub2(allocatable_str)
 end program 