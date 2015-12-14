program bench_maps
  implicit none
  BEGIN_DOC
! Performs timing benchmarks on integral access
  END_DOC
  integer                        :: i,j,k,l
  integer*8                      :: ii,jj
  double precision               :: r, cpu
  integer*8                      :: cpu0, cpu1, count_rate, count_max
  
  PROVIDE mo_bielec_integrals_in_map
  print *,  mo_tot_num, 'MOs'

  ! Time random function
  call system_clock(cpu0, count_rate, count_max)
  do ii=1,100000000_8
    call random_number(r)
    i = int(r*mo_tot_num)+1
    call random_number(r)
    j = int(r*mo_tot_num)+1
    call random_number(r)
    k = int(r*mo_tot_num)+1
    call random_number(r)
    l = int(r*mo_tot_num)+1
  enddo
  call system_clock(cpu1, count_rate, count_max)
  cpu = (cpu1-cpu0)/count_rate
  print *, 'Random function : ', cpu/dble(ii)

  call system_clock(cpu0, count_rate, count_max)
  do ii=1,100000000_8
    call random_number(r)
    i = int(r*mo_tot_num)+1
    call random_number(r)
    j = int(r*mo_tot_num)+1
    call random_number(r)
    k = int(r*mo_tot_num)+1
    call random_number(r)
    l = int(r*mo_tot_num)+1
    call get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
  enddo
  call system_clock(cpu1, count_rate, count_max)
  cpu = -cpu + (cpu1 - cpu0)/count_rate
  print *, 'Random access : ', cpu/dble(ii)

  ii=0_8
  call system_clock(cpu0, count_rate, count_max)
  do jj=1,10
  do l=1,mo_tot_num
    do k=1,mo_tot_num
      do j=1,mo_tot_num
        do i=1,mo_tot_num
          ii += 1
          call get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
        enddo
      enddo
    enddo
  enddo
  enddo
  call system_clock(cpu1, count_rate, count_max)
  cpu = (cpu1 - cpu0)/count_rate
  print *, 'loop ijkl : ', cpu/dble(ii)

  ii=0
  call system_clock(cpu0, count_rate, count_max)
  do jj=1,10
  do l=1,mo_tot_num
    do j=1,mo_tot_num
      do k=1,mo_tot_num
        do i=1,mo_tot_num
          ii += 1
          call get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
        enddo
      enddo
    enddo
  enddo
  enddo
  call system_clock(cpu1, count_rate, count_max)
  cpu = (cpu1 - cpu0)/count_rate
  print *, 'loop ikjl : ', cpu/dble(ii)

  ii=0
  call system_clock(cpu0, count_rate, count_max)
  do jj=1,10
  do k=1,mo_tot_num
    do l=1,mo_tot_num
      do j=1,mo_tot_num
        do i=1,mo_tot_num
          ii += 1
          call get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
        enddo
      enddo
    enddo
  enddo
  enddo
  call system_clock(cpu1, count_rate, count_max)
  cpu = (cpu1 - cpu0)/count_rate
  print *, 'loop ijlk : ', cpu/dble(ii)

  ii=0
  call system_clock(cpu0, count_rate, count_max)
  do jj=1,10
  do i=1,mo_tot_num
    do j=1,mo_tot_num
      do k=1,mo_tot_num
        do l=1,mo_tot_num
          ii += 1
          call get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
        enddo
      enddo
    enddo
  enddo
  enddo
  call system_clock(cpu1, count_rate, count_max)
  cpu = (cpu1 - cpu0)/count_rate
  print *, 'loop lkji : ', cpu/dble(ii)

  ii=0
  call system_clock(cpu0, count_rate, count_max)
  do jj=1,10
  do j=1,mo_tot_num
    do i=1,mo_tot_num
      do k=1,mo_tot_num
        do l=1,mo_tot_num
          ii += 1
          call get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
        enddo
      enddo
    enddo
  enddo
  enddo
  call system_clock(cpu1, count_rate, count_max)
  cpu = (cpu1 - cpu0)/count_rate
  print *, 'loop lkij : ', cpu/dble(ii)

end
