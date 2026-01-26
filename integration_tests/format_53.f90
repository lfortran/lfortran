program format_53
  ! Test TR (tab right), TL (tab left), and T (absolute tab) format descriptors
  implicit none
  character(12):: str1 = 'abcdefghijkl', str2
  character(4) :: str3
  character(8) :: str4
  
  ! Test 1: TR (tab right) - skip first 4 characters
  read(str1,"(tr4,a)") str2
  print "(A)", '"'//str2//'"'
  if (str2 /= 'efghijkl    ') error stop "Test 1 failed: TR"
  
  ! Test 2: TL (tab left) - move forward then back
  ! TR8 moves to position 8, TL4 moves back 4 positions to position 4
  read(str1,"(tr8,tl4,a)") str2
  print "(A)", '"'//str2//'"'
  if (str2 /= 'efghijkl    ') error stop "Test 2 failed: TL"
  
  ! Test 3: Multiple TR operations
  read(str1,"(tr2,tr2,a)") str2
  print "(A)", '"'//str2//'"'
  if (str2 /= 'efghijkl    ') error stop "Test 3 failed: Multiple TR"
  
  ! Test 4: TR followed by TL to read from beginning
  read(str1,"(tr6,tl6,a)") str2
  print "(A)", '"'//str2//'"'
  if (str2 /= 'abcdefghijkl') error stop "Test 4 failed: TR+TL to start"
  
  ! Test 5: TR to skip, read partial, then more operations
  read(str1,"(tr3,a4)") str3
  print "(A)", '"'//str3//'"'
  if (str3 /= 'defg') error stop "Test 5 failed: TR with width"
  
  ! Test 6: T (absolute tab) - tab to position 5
  read(str1,"(t5,a)") str2
  print "(A)", '"'//str2//'"'
  if (str2 /= 'efghijkl    ') error stop "Test 6 failed: T absolute"
  
  ! Test 7: T to position 1 (beginning)
  read(str1,"(t1,a)") str2
  print "(A)", '"'//str2//'"'
  if (str2 /= 'abcdefghijkl') error stop "Test 7 failed: T to start"
  
  ! Test 8: T to middle, read partial
  read(str1,"(t4,a4)") str3
  print "(A)", '"'//str3//'"'
  if (str3 /= 'defg') error stop "Test 8 failed: T with width"
  
  ! Test 9: Combination of T and TR
  read(str1,"(t3,tr2,a8)") str4
  print "(A)", '"'//str4//'"'
  if (str4 /= 'efghijkl') error stop "Test 9 failed: T+TR combo"
  
  ! Test 10: T followed by TL
  read(str1,"(t7,tl3,a4)") str3
  print "(A)", '"'//str3//'"'
  if (str3 /= 'defg') error stop "Test 10 failed: T+TL combo"
  
  print *, "All tests passed"
end program format_53

