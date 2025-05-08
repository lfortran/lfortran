

module string_52_mod
    contains 
    subroutine sub(x, char)
        integer :: x
        character(x),optional :: char
    end subroutine 
end module
  
  program string_52
    use string_52_mod
    call sub(10)
  end program 
  