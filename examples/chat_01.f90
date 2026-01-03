program chat_01
    implicit none
    integer :: choice
    character(len = 10) :: name

    print *, "Please enter your name:"
    read(*, *) name

    print *, "---------WELCOME TO CHAT_01---------"
    do while (.true.)
        print *, "Please pick an option:"
        print *, "1) Say Hi"
        print *, "2) Say Hello"
        print *, "3) Say Good Morning"
        print *, "4) Say Happy Birthday"
        print *, "5) Exit the chat"
        print *, ""

        print *, "Enter your choice:"
        read(*, *) choice

        if (choice == 1) then
            print *, "Hi "//name
        else if (choice == 2) then
            print *, "Hello "//name
        else if (choice == 3) then
            print *, "Good Morning "//name
        else if (choice == 4) then
            print *, "Happy Birthday "//name//"!"
        else if (choice == 5) then
            exit
        else
            print *, "Wrong choice. Try again..."
        end if

        print *, ""
    end do
end program
