program game2048
    implicit none

    integer, parameter :: rows = 4
    integer, parameter :: cols = 4
    integer :: myArray(rows, cols)
    character :: key
    integer :: score = 0
    integer :: gameStatus = 0

    call fillArray(myArray)
    call printArray(myArray)
    do while (gameStatus == 0)
        read *, key
        select case (trim(key))
            case ('w')  ! Up arrow key
                print *, "Up arrow key pressed"
                call up(myArray)
            case ('s')  ! Down arrow key
                print *, "Down arrow key pressed"
                call down(myArray)
            case ('d')  ! Right arrow key
                print *, "Right arrow key pressed"
                call right(myArray)
            case ('a')  ! Left arrow key
                print *, "Left arrow key pressed"
                call left(myArray)
            case default
                print *, "w-up, s-down, a-left, d-right"
        end select
        call addNewNumber(myArray)
        call printArray(myArray)
        gameStatus = checkWinLose(myArray)
    end do
    if (gameStatus == 1) then
        print *, "You WIN! Your score is ", score
    else if (gameStatus == -1) then
        print *, "You LOSE! Your score is ", score
    end if


contains
    subroutine addNewNumber(arr)
        integer, intent(inout) :: arr(:,:)
        integer :: i, j, added, possible
        real :: randReal
        added = 0
        possible = 1
        do while (added == 0 .and. possible .ne. 0)
            possible = 0
            do i = 1, rows
                do j = 1, cols
                    call random_number(randReal)
                    if (arr(i, j) == 0) then
                        possible = 1
                        if (randReal < 0.3 .and. added == 0) then
                            arr(i, j) = generateRandomNumber()
                            added = 1
                            exit
                        end if
                    end if
                end do
            end do
        end do
    end subroutine addNewNumber

    subroutine left(arr)
        integer, intent(inout) :: arr(:,:)
        integer :: i, j, move, sum
        move = 1
        do while (move .ne. 0)
            move = 0
            do i = 1, rows
                do j = 1, cols
                    if (j .ne. 1) then
                        if (arr(i, j) .ne. 0) then
                            if (arr(i, j-1) == arr(i, j)) then
                                sum = arr(i, j-1) * 2
                                arr(i, j-1) = sum
                                score = score + sum
                                arr(i, j) = 0
                                move = 1
                            else if (arr(i, j-1) == 0) then
                                arr(i, j-1) = arr(i, j)
                                arr(i, j) = 0
                                move = 1
                            end if
                        end if
                    end if
                end do
            end do
        end do
    end subroutine left

    subroutine right(arr)
        integer, intent(inout) :: arr(:,:)
        integer :: i, j, move, sum
        move = 1
        do while (move .ne. 0)
            move = 0
            do i = 1, rows
                do j = cols, 1, -1
                    if (j .ne. cols) then
                        if (arr(i, j) .ne. 0) then
                            if (arr(i, j+1) == arr(i, j)) then
                                sum = arr(i, j+1) * 2
                                arr(i, j+1) = sum
                                score = score + sum
                                arr(i, j) = 0
                                move = 1
                            else if (arr(i, j+1) == 0) then
                                arr(i, j+1) = arr(i, j)
                                arr(i, j) = 0
                                move = 1
                            end if
                        end if
                    end if
                end do
            end do
        end do
    end subroutine right

    subroutine up(arr)
        integer, intent(inout) :: arr(:,:)
        integer :: i, j, move, sum
        move = 1
        do while (move .ne. 0)
            move = 0
            do i = 1, rows
                do j = 1, cols
                    if (i .ne. 1) then
                        if (arr(i, j) .ne. 0) then
                            if (arr(i-1, j) == arr(i, j)) then
                                sum = arr(i-1, j) * 2
                                arr(i-1, j) = sum
                                score = score + sum
                                arr(i, j) = 0
                                move = 1
                            else if (arr(i-1, j) == 0) then
                                arr(i-1, j) = arr(i, j)
                                arr(i, j) = 0
                                move = 1
                            end if
                        end if
                    end if
                end do
            end do
        end do
    end subroutine up

    subroutine down(arr)
        integer, intent(inout) :: arr(:,:)
        integer :: i, j, move, sum
        move = 1
        do while (move .ne. 0)
            move = 0
            do i = rows, 1, -1
                do j = 1, cols
                    if (i .ne. rows) then
                        if (arr(i, j) .ne. 0) then
                            if (arr(i+1, j) == arr(i, j)) then
                                sum = arr(i+1, j) * 2
                                arr(i+1, j) = sum
                                score = score + sum
                                arr(i, j) = 0
                                move = 1
                            else if (arr(i+1, j) == 0) then
                                arr(i+1, j) = arr(i, j)
                                arr(i, j) = 0
                                move = 1
                            end if
                        end if
                    end if
                end do
            end do
        end do
    end subroutine down

    function generateRandomNumber() result(randNum)
        implicit none
        integer :: randNum
        real :: randReal
        call random_number(randReal)
        if (randReal < 0.75) then
            randNum = 2
        else
            randNum = 4
        end if
    end function generateRandomNumber

    subroutine fillArray(arr)
        integer, intent(inout) :: arr(:,:)
        integer :: i, j
        integer :: count
        real :: randReal
        count = 0
        do while (count < 2)
            count = 0
            do i = 1, rows
                do j = 1, cols
                    call random_number(randReal)
                    if (count < 2 .and. randReal < 0.3) then
                        arr(i, j) = generateRandomNumber()
                        count = count + 1
                    else
                        arr(i, j) = 0
                    end if
                end do
            end do
        end do
    end subroutine fillArray

    subroutine printArray(arr)
        integer, intent(in) :: arr(:,:)
        print *, "Score: ", score
        print *, " ______ ______ ______ ______ "
        print *, "|      |      |      |      |"
        print *, "|", transformToCell(arr(1,1)), transformToCell(arr(1,2)), transformToCell(arr(1,3)), transformToCell(arr(1,4))
        print *, "|______|______|______|______|"
        print *, "|      |      |      |      |"
        print *, "|", transformToCell(arr(2,1)), transformToCell(arr(2,2)), transformToCell(arr(2,3)), transformToCell(arr(2,4))
        print *, "|______|______|______|______|"
        print *, "|      |      |      |      |"
        print *, "|", transformToCell(arr(3,1)), transformToCell(arr(3,2)), transformToCell(arr(3,3)), transformToCell(arr(3,4))
        print *, "|______|______|______|______|"
        print *, "|      |      |      |      |"
        print *, "|", transformToCell(arr(4,1)), transformToCell(arr(4,2)), transformToCell(arr(4,3)), transformToCell(arr(4,4))
        print *, "|______|______|______|______|"
        print *, ""
    end subroutine printArray

    function transformToCell(number) result(string)
        integer, intent(in) :: number
        character(len=7) :: string
        if (number == 0) then
            string = "      |"
        else if (number == 2) then
            string = "  2   |"
        else if (number == 4) then
            string = "  4   |"
        else if (number == 8) then
            string = "  8   |"
        else if (number == 16) then
            string = "  16  |"
        else if (number == 32) then
            string = "  32  |"
        else if (number == 64) then
            string = "  64  |"
        else if (number == 128) then
            string = "  128 |"
        else if (number == 256) then
            string = "  256 |"
        else if (number == 512) then
            string = "  512 |"
        else if (number == 1024) then
            string = " 1024 |"
        else if (number == 2048) then
            string = " 2048 |"
        end if
        return

    end function transformToCell

    function checkWinLose(arr) result(status)
        integer, intent(in) :: arr(:,:)
        integer :: status ! 1 - win, -1 - lose,  0 - in progress
        integer :: i, j, count
        status = 0
        count = 0
        do i = 1, rows
            do j = 1, cols
                if (arr(i, j) == 2048) then
                    status = 1
                    return
                else if (arr(i, j) == 0) then
                    count = count + 1
                end if
            end do
        end do
        if (count == 0) then ! no space, check if you can do move
            status = -1
            do i = 1, rows
                do j = 1, cols
                    if (j .ne. cols) then
                        if (arr(i, j) == arr(i, j+1)) then
                            status = 0
                            return
                        end if
                    end if
                    if (i .ne. rows) then
                        if (arr(i, j) == arr(i+1, j)) then
                            status = 0
                            return
                        end if
                    end if
                end do
            end do
        end if
    end function checkWinLose
end program game2048
