module rng ! compile with gfortran -c -O0 -std=f2008 rng.f90
    implicit none
    integer, parameter :: srk = selected_real_kind(15, 300)

contains
function lin_cong(X, A, B, M)
    implicit none
    real(kind = srk), intent(in) :: X, A, B, M ! X = seed value; A, B are for determining equation of line of numbers
    real(kind = srk) :: lin_cong

    lin_cong = mod(A*X + B, M) ! Returns the 'remainder' of the number divided by M
end function

function box_muller(array, N, mean, std_deviation) ! calculates normally-distributed numbers
    implicit none
    real(kind = srk), intent(in) :: mean, std_deviation ! stats of the array's numbers
    integer, intent(in) :: N ! array size
    real(kind = srk), dimension(N), intent(in) :: array ! uniformly distributed numbers
    real(kind = srk), dimension(N) :: box_muller ! final result is an N-sized array containing the generated numbers
    real(kind = srk) :: pi = 4.0*atan(1.0) ! dummy variables for calculations
    integer :: i

    do i = 1, N - 1 ! iterates to calculate a pair of numbers, using sin() and cos()
        box_muller(i) = std_deviation*sqrt(-2.0_srk*log(array(i)))*cos(2.0_srk*pi*array(i + 1)) + mean
        box_muller(i + 1) = std_deviation*sqrt(-2.0_srk*log(array(i)))*sin(2.0_srk*pi*array(i + 1)) + mean
    end do
end function

function quad_cong(X, A, B, C, M)
    implicit none
    real(kind = srk), intent(in) :: X, A, B, C, M ! X = seed value, A, B = parameters for equation of line of numbers
    real(kind = srk) :: quad_cong

    quad_cong = mod(A*X**2 + B*X + C, M) ! takes remainder of quadratically-calculated number divided by M
end function

function stats(X, N) ! calculates mean, variance, and standard error of an array of generated numbers
    implicit none
    integer, intent(in) :: N
    real(kind = srk), dimension(N), intent(in) :: X
    real(kind = srk) :: sum, sq_sum
    integer :: i
    real(kind = srk), dimension(3) :: stats

    do i = 1, N
        sum = sum + X(i)
        sq_sum = sq_sum + X(i)**2
    end do

    stats(1) = sum/real(N, kind = srk) ! mean
    stats(2) = (sq_sum/real(N, kind = srk)) - stats(1)**2 ! variance
    stats(3) = sqrt(stats(2)/real(N, kind = srk)) ! standard error
end function
end module