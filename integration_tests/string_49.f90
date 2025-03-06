module string_49_mod
   implicit none
   private
   public getopt, str
   character(len=128) :: str = ''
   contains
      subroutine getopt() 
        character(len=10) :: status 
        if (str(1:128) == '-') then
            status =  "fail"
        else
            status = "success"
        end if
        if (status /= "success") error stop
      end subroutine getopt

end module string_49_mod


program string_49 
    use string_49_mod 
    character(len=10) :: status 
    character(len=128) :: str2 = '' 
    call getopt()
    if (str(1:128) == '-') then
        status =  "fail"
    else
        status = "success"
    end if
    if (status /= "success") error stop
    if (str2(1:128) == '-') then
        status =  "fail"
    else
        status = "success"
    end if
    if (status /= "success") error stop
end program 