subroutine b()
   external f
   call a(f)
end subroutine

subroutine a(f)
   real r
   external f
   r = f(2.0)
   print *, r
end subroutine

program external_03
   call b()
end program
