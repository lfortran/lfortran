subroutine show
print *, 'ok'
end subroutine

program external_call_01
external show
call show()
end program
