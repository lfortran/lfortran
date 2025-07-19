!Test global array of strings
module arrays_97_mod
    character(10) :: arr(2)
end module 
  program arrays_97
    use arrays_97_mod
     arr(1) = "aa"
     print *, arr(1)
     if(arr(1) /= "aa") error stop
  end program