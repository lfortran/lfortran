program procedure_11
   procedure(say_something), pointer :: proc
   proc => say_something
   call proc("Hello from proc!")
contains
   subroutine say_something(message)
      character(len=*),intent(in) :: message
      print *, message
      if (message /= "Hello from proc!") error stop
   end subroutine
end program procedure_11