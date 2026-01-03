program string_85
call test_func()
call test_func()
print *, "Passed"

contains

    subroutine test_func()
    character(10), save :: ch = '0123456789'
    integer, save :: call_count = 0
    call_count = call_count + 1
    print *, "ch before: '", ch, "'"
    if (call_count == 1) then
        if (ch /= '0123456789') error stop 'First call: initial value incorrect'
    else if (call_count == 2) then
        if (ch /= '0123X56789') error stop 'Second call: value not persisted from first call'
    end if
    ch(5:5) = 'X'
    print *, "ch after:  '", ch, "'"
    if (ch /= '0123X56789') error stop 'Modified value incorrect'
    end subroutine test_func

end program
