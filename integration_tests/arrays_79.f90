module arrays_79_mod
    contains 
    subroutine ff(q)
      integer, intent(in) :: q(:,:)
      integer :: anew(size(q))
      integer :: anew2(4)
      anew2 = [q]
      print *, anew2
      anew = reshape(anew2, [size(q)])
      print *, anew
      if(any(anew /= [1,2,3,4])) error stop 
    end subroutine ff
end module arrays_79_mod
  
  program arrays_79
    use arrays_79_mod
    integer :: arr(2,2)
    arr = reshape([1,2,3,4], [2,2])
    call ff(arr)
  end program arrays_79