module arrays_reshape_19_mod
    contains 
    subroutine another_func(q)
      integer, intent(in) :: q(:,:)
      integer :: anew(18)
      anew = reshape([0,q,17], [18])
      print *, anew
      if(any(anew /= [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17])) error stop 
    end subroutine another_func
  end module
  
  program arrays_reshape_19
    use arrays_reshape_19_mod
    integer :: q(4,4)
    q = reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], [4,4])
    call another_func(q)
  end program arrays_reshape_19