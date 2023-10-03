program write4
    implicit none

    write (*, "(a)", advance="no") "hi, "
    write (*, "(a)") "how are you?"

    write (*, "(a)", advance="yes") "I "
    write (*, "(a)", advance="yes") "am"
    write (*, "(a)", advance="no") "doing "
    write (*, "(a)", advance="yes") "good"
end program
