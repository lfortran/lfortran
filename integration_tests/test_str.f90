! Testing Python's str casting
! -- Example --
! i = 10
! s = "Hi"
! s = str(i) # Equivalent in LFortran is `lfortran_str()`

program test_str
    character(30) :: str
    integer :: i
    real  :: r

    i = 11
    str =  "HI"//_lfortran_str(9) // _lfortran_str(i)
    print *, str, "|"
    if(trim(str) /= "HI911") error stop
  ! --------------------------------------------------------------------------- !
    
    r = 2.3
    str =  trim(str) // "World" //_lfortran_str(r) // "--"// _lfortran_str(22.5) 
    print *, str,"|"
    if(trim(str) /= "HI911World2.300000--22.500000") error stop
    
    ! --------------------------------------------------------------------------- !
    
    call test(_lfortran_str(112233) ,"112233")
    call test(_lfortran_str(i) ,"11")
    
    
    ! --------------------------------------------------------------------------- !
    ! --------------------------------------------------------------------------- !
    ! --------------------------------------------------------------------------- !

    CONTAINS
    
    subroutine test (str_from_cast, test_against_str)
        character(*) :: str_from_cast
        character(*) :: test_against_str

        print *, len(str_from_cast)
        print *, str_from_cast

        print *, len(test_against_str)
        print *, test_against_str

        if(str_from_cast /= test_against_str) error stop
    end subroutine
  
  end program