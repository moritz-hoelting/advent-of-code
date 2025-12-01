program part1
    implicit none

    character(len=20) :: line
    character :: dir
    integer :: value
    integer :: ios
    integer :: count, dial

    open(unit=10, file="input.txt", status="old", action="read")

    count = 0
    dial = 50

    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit

        dir = line(1:1)
        read(line(2:), *) value

        if (dir == 'R') then
            dial = mod(dial + value, 100)
            ! print '(A,1X,I0)', "Move right:", value
        else if (dir == 'L') then
            dial = mod(dial - value + 100, 100)
            ! print '(A,1X,I0)', "Move left:", value
        end if
        if (dial == 0) count = count + 1
    end do

    print '(A,1X,I0)', "End dial value: ", dial
    print '(A,1X,I0)', "Total times dial was zero: ", count

    close(10)

end program part1