module present_07_mod

  abstract interface
    subroutine my_fun(x)
        real, intent(in) :: x
    end subroutine my_fun
  end interface

  contains

  subroutine with_fun(sub)
     procedure(my_fun), optional :: sub
     if (present(sub)) error stop "Expected not present"
  end subroutine

  subroutine with_fun_present(sub)
     procedure(my_fun), optional :: sub
     if (.not. present(sub)) error stop "Expected present"
  end subroutine

  subroutine real_sub(x)
     real, intent(in) :: x
  end subroutine

end module present_07_mod

program present_07
  use present_07_mod

  procedure(my_fun), pointer :: ptr_null => null()
  procedure(my_fun), pointer :: ptr_assoc

  ptr_assoc => real_sub

  call with_fun()
  call with_fun(sub=ptr_null)
  call with_fun_present(sub=ptr_assoc)
  call with_fun_present(sub=real_sub)

  print *, "PASS"
end program present_07
