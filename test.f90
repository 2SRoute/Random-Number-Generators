program test ! compile using 'gfortran -g -Wall -fcheck=all -fmax-errors=1 -std=f2008 rng.o test.f90
    use rng
    implicit none
    real(kind = srk), parameter :: A = 1570, B = 2864, M = 2**32 - 1 ! rng parameters
    real(kind = srk) :: X = 2007 ! seeds
    integer, parameter :: N = 10000 ! total number of numbers to generate
    integer :: i, unit = 50
    real(kind = srk), dimension(:), allocatable :: Xvals, Xstats ! storage arrays for holding numbers
    real(kind = srk) :: min, max, width ! parameters for binning data
    integer :: label, bins = 100
    integer(kind = srk), dimension(:), allocatable :: uniform, normal ! bins for uniformly- and normally-distributed numbers

    allocate(Xvals(N))
    do i = 1, N ! iterates up to N, generating two numbers to be stored in an array for later
        X = lin_cong(X, A, B, M)
        Xvals(i) = X/M ! normalises the number and stores it in the array
    end do

    allocate(Xstats(3)) ! calculates stats of the generated numbers, prints to screen
    Xstats = stats(Xvals, N)
    print*, "Linear-congruential Algorithm:"
    print*, "Mean: ", Xstats(1)
    print*, "Variance: ", Xstats(2)
    print*, "Standard Error: ", Xstats(3)
    print*, " "
    Xvals = box_muller(Xvals, N, Xstats(1), sqrt(Xstats(2))) ! uses the box-muller method to generate numbers

    min = Xstats(1) - 3.0_srk*sqrt(Xstats(2)) ! minimum, within 3 deviations of the mean
    max = Xstats(1) + 3.0_srk*sqrt(Xstats(2)) ! maximum, within same deviations of mean
    width = (max - min)/real(bins, kind = srk) ! bin width

    allocate(uniform(N))
    do i = 1, N
        label = int((Xvals(i) - min)/width) + 1 ! bins values, incrementing the bin counter by 1
        uniform(label) = uniform(label) + 1 ! whenever a value falls into the corresponding bin
    end do

    open(unit, file = 'uniform_distribution.txt', status = 'replace', position = 'rewind', action = 'write')
    do i = 0, bins - 1 ! writes binned data to a .txt file
        write(unit, fmt = *) (min + (i - 0.5_srk)*width), ", ", uniform(i)/(real(N, kind = srk)*width)
    end do
    close(unit)

    Xstats = stats(Xvals, N) ! calculates stats, prints to screen
    print*, "Box-Muller Algorithm:"
    print*, "Mean: ", Xstats(1)
    print*, "Variance: ", Xstats(2)
    print*, "Standard Error: ", Xstats(3)

    min = Xstats(1) - 3.0_srk*sqrt(Xstats(2)) ! same process for binning below, but for the box-muller numbers
    max = Xstats(1) + 3.0_srk*sqrt(Xstats(2))
    width = (max - min)/real(bins, kind = srk)

    allocate(normal(N))
    do i = 1, N
        label = int((Xvals(i) - min)/width) + 1
        uniform(label) = uniform(label) + 1
    end do
end program