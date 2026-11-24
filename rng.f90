module rng ! compile with gfortran -c -O0 rng.f90
    implicit none
    integer, parameter :: srk = selected_real_kind(15, 300)

contains
function lin_cong(X, A, B, M)
    implicit none
    real(kind = srk), intent(inout) :: X ! seed value
    real(kind = srk), intent(in) :: A, B, M ! A, B = for determining equation of line of numbers
    real(kind = srk) :: lin_cong

    lin_cong = mod(A*X + B, M) ! Returns the 'remainder' of the number divided by M
end function

function stats(X, N) ! calculates mean, variance, and standard error of an array of generated numbers
    implicit none
    real(kind = srk), dimension(N) :: X
    integer, intent(in) :: N
    real(kind = srk) :: sum, sq_sum
    integer :: i
    real(kind = srk), dimension(3) :: stats

    do i = 1, N
        sum = sum + X(i)
        sq_sum = sq_sum + X(i)**2
    end do

    stats(1) = sum/real(N, kind = srk) ! mean
    stats(2) = (sq_sum/real(N, kind = srk)) - stats(1) ! variance
    stats(3) = sqrt(stats(2)/real(N, kind = srk)) ! standard error
end function
end module