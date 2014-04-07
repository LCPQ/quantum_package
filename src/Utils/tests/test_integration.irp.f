program test_integration
  implicit none
  character*(128)                :: arg
  integer                        :: iargc
  integer                        :: i
  if (iargc() < 1) then
    print *,  iargc()
    print *, 'usage : test_integration <test_name>'
    stop 1
  endif
  call getarg(1,arg)
  i = len(arg)
  do while (arg(i:i) == ' ')
    i -= 1
    if (i == 0) then
      stop 1
    endif
  enddo
  i -= 1
  do while (arg(i:i) /= '/')
    i -= 1
    if (i == 0) then
      stop 1
    endif
  enddo
  i += 1
  arg = arg(i:)
  BEGIN_SHELL [ /bin/bash ]
  for i in $(grep subroutine tests/test_integration.irp.f | cut -d ' ' -f 2 | sed 's/test_//' )
  do
    echo "if (trim(arg) == '"$i"') then"
    echo ' call test_'$i
    echo 'endif'
  done
  END_SHELL
end

subroutine test_rint1
  implicit none
  integer                        :: i,j
  double precision               :: rho(10)
  double precision               :: rint1
  do i=1,size(rho)
    rho(i) = 2.d0**(1-i)
  enddo
  do j=1,size(rho)
    do i=0,8
      print '(I2,A,F12.8,A3,E15.8)', i, ',', rho(j), ' : ', rint1(i,rho(j))
    enddo
  enddo
end

subroutine test_rint_large_n
  implicit none
  integer                        :: i,j
  double precision               :: rho(10)
  double precision               :: rint_large_n
  do i=1,size(rho)
    rho(i) = 2.d0**(2-i)
  enddo
  do j=1,size(rho)
    do i=4,20
      print '(I2,A,F12.8,A3,E15.8)', i, ',', rho(j), ' : ', rint_large_n(i,rho(j))
    enddo
  enddo
end

subroutine test_hermite
  implicit none
  integer                        :: i,j
  double precision               :: x(10)
  double precision               :: hermite
  do i=1,size(x)
    x(i) = (-1.d0)**i * 2.d0**(5-i)
  enddo
  do j=1,size(x)
    do i=0,10
      print '(I2,A,F12.8,A3,E15.8)', i, ',', x(j), ' : ', hermite(i,x(j))
    enddo
  enddo
end

subroutine test_rint_sum
  implicit none
  integer                        :: i,j
  double precision               :: d1(0:50), rho(10)
  double precision               :: rint_sum
  do i=0,size(d1)-1
    d1(i) = (-1.d0)**i * 2.d0**(5-i)
  enddo
  do i=1,size(rho)
    rho(i) = abs(1.d0/d1(i))
  enddo
  do j=1,size(rho)
    do i=0,5
      print '(I2,A,F12.8,A3,E15.8)', 4*i+1, ',', rho(j), ' : ', rint_sum(4*i+1,rho(j),d1)
    enddo
  enddo
end

subroutine test_rint
  implicit none
  integer                        :: i,j
  double precision               :: rho(10)
  double precision               :: rint
  do i=1,size(rho)
    rho(i) = 2.d0**(2-i)
  enddo
  do j=1,size(rho)
    do i=0,20,3
      print '(I2,A,F12.8,A3,E15.8)', i, ',', rho(j), ' : ', rint(i,rho(j))
    enddo
  enddo
end

subroutine test_F_integral
  implicit none
  integer                        :: i,j
  double precision               :: rho(10)
  double precision               :: F_integral
  do i=1,size(rho)
    rho(i) = 2.d0**(2-i)
  enddo
  do j=1,size(rho)
    do i=0,20,3
      print '(I2,A,F12.8,A3,E15.8)', i, ',', rho(j), ' : ', F_integral(i,rho(j))
    enddo
  enddo

end
