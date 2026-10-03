module module_string_113
    implicit none
    contains
    
    function atleast(line, length, pattern) result(strout)
       character(len=*), intent(in) :: line
       integer, intent(in) :: length
       character(len=*), intent(in), optional :: pattern
       character(len=max(length, len(trim(line)))) :: strout
       if(present(pattern)) then
          strout = line//repeat(pattern, len(strout)/len(pattern)+1)
       else
          strout = line
       end if
    end function atleast
    
end module module_string_113
    
program string_113
       use module_string_113
       implicit none
       character(len=:), allocatable :: res
    
       ! No pattern: result length = max(length, len(trim(line)))
       ! 'hello' has trim-len 5, max(3,5)=5 → result is 'hello'
       res = atleast('hello', 3)
       if (len(res) /= 5) error stop
       if (res /= 'hello') error stop
    
       ! 'hi' has trim-len 2, max(8,2)=8 → 'hi' padded with spaces to length 8
       res = atleast('hi', 8)
       if (len(res) /= 8) error stop
       if (res /= 'hi      ') error stop
    
       ! With pattern: 'hi' trim-len 2, max(5,2)=5, strout='hi'//repeat('-',6) truncated to 5
       res = atleast('hi', 5, '-')
       if (len(res) /= 5) error stop
       if (res /= 'hi---') error stop
    
       ! With pattern: line already fills result ('hello' trim-len 5, max(3,5)=5)
       res = atleast('hello', 3, '*')
       if (len(res) /= 5) error stop
       if (res /= 'hello') error stop
    
       ! With 2-char pattern: 'a' trim-len 1, max(6,1)=6, 'a'//repeat('bc',4)='abcbcbcbc' → 'abcbcb'
       res = atleast('a', 6, 'bc')
       if (len(res) /= 6) error stop
       if (res /= 'abcbcb') error stop
end program string_113