program main
implicit none
integer, parameter :: n1 = 2, n2 = 3
integer            :: i,mat(n1,n2)
mat(1,:) = [2,3,4]
mat(2,:) = [3,4,5]
do i=1,n1
   write (*,"(*(1x,i6))") mat(i,:) ! unlimited format used, with 6 positions for each integer, separated by a space
end do
write (*,"(3(1x,i6))") transpose(mat) ! Alternative using format reversion. Transpose used because Fortran arrays are column major.
end program main
