subroutine f()
end subroutine

subroutine g()
end subroutine

subroutine setulb(task)
character*60 task
print *, task
if (task /= 'START') error stop

contains
subroutine x()
end subroutine x
end subroutine setulb

program set_bindc_01
    character*60 task
    task = 'START'
    call setulb(task)
end program
