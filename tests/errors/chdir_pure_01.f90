! CHDIR is impure, so a pure subroutine cannot call it (#12257).
pure subroutine puresub
  intrinsic sleep, chdir, get_command
  call chdir('.')  ! {Error} Call to impure procedure 'chdir' is not allowed inside a PURE procedure
end
