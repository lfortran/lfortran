program select_label_01
    implicit none
    integer :: choice
    choice = 2
SELECT_BLOCK: select case (choice)
    case (1)
    case default
        INNER: block
            choice = 3
            exit SELECT_BLOCK
            choice = 4
        end block INNER
end select SELECT_BLOCK
print *, "Choice is: ", choice
if (choice /= 3) error stop
end program select_label_01
