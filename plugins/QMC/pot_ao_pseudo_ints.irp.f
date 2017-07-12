
BEGIN_PROVIDER [ double precision, ao_pseudo_grid, (ao_num,-pseudo_lmax:pseudo_lmax,0:pseudo_lmax,nucl_num,pseudo_grid_size)  ]
 implicit none
 BEGIN_DOC
! Grid points for f(|r-r_A|) = \int Y_{lm}^{C} (|r-r_C|, \Omega_C) \chi_i^{A} (r-r_A) d\Omega_C
!
! <img src="http://latex.codecogs.com/gif.latex?f(|r-r_A|)&space;=&space;\int&space;Y_{lm}^{C}&space;(|r-r_C|,&space;\Omega_C)&space;\chi_i^{A}&space;(r-r_A)&space;d\Omega_C"
! title="f(|r-r_A|) = \int Y_{lm}^{C} (|r-r_C|, \Omega_C) \chi_i^{A} (r-r_A) d\Omega_C" />
 END_DOC
 ! l,m    : Y(l,m) parameters
 ! c(3)   : pseudopotential center
 ! a(3)   : Atomic Orbital center
 ! n_a(3) : Powers of x,y,z in the Atomic Orbital
 ! g_a    : Atomic Orbital exponent      
 ! r      : Distance between the Atomic Orbital center and the considered point
 double precision, external :: ylm_orb
 integer :: n_a(3)
 double precision :: a(3), c(3), g_a
 integer :: i,j,k,l,m,n,p
 double precision :: dr, Ulc
 double precision :: y
 double precision, allocatable :: r(:)

 allocate (r(pseudo_grid_size))
 dr = pseudo_grid_rmax/dble(pseudo_grid_size)
 r(1) = 0.d0
 do j=2,pseudo_grid_size
   r(j) = r(j-1) + dr
 enddo

 ao_pseudo_grid = 0.d0
 do j=1,pseudo_grid_size
   do k=1,nucl_num
     c(1:3) = nucl_coord(k,1:3)
     do l=0,pseudo_lmax
       do i=1,ao_num
         a(1:3) = nucl_coord(ao_nucl(i),1:3)
         n_a(1:3) = ao_power(i,1:3)
         do p=1,ao_prim_num(i)
           g_a = ao_expo_ordered_transp(p,i)
           do m=-l,l
             y = ylm_orb(l,m,c,a,n_a,g_a,r(j))
             ao_pseudo_grid(i,m,l,k,j) = ao_pseudo_grid(i,m,l,k,j) + &
                 ao_coef_normalized_ordered_transp(p,i)*y
           enddo
         enddo
       enddo
     enddo
   enddo
 enddo
 deallocate(r)

END_PROVIDER


BEGIN_PROVIDER [ double precision, mo_pseudo_grid, (ao_num,-pseudo_lmax:pseudo_lmax,0:pseudo_lmax,nucl_num,pseudo_grid_size)  ]
 implicit none
 BEGIN_DOC
! Grid points for f(|r-r_A|) = \int Y_{lm}^{C} (|r-r_C|, \Omega_C) \phi_i^{A} (r-r_A) d\Omega_C
!
! <img src="http://latex.codecogs.com/gif.latex?f(|r-r_A|)&space;=&space;\int&space;Y_{lm}^{C}&space;(|r-r_C|,&space;\Omega_C)&space;\chi_i^{A}&space;(r-r_A)&space;d\Omega_C"
! title="f(|r-r_A|) = \int Y_{lm}^{C} (|r-r_C|, \Omega_C) \chi_i^{A} (r-r_A) d\Omega_C" />
 END_DOC
 ! l,m    : Y(l,m) parameters
 ! c(3)   : pseudopotential center
 ! a(3)   : Atomic Orbital center
 ! n_a(3) : Powers of x,y,z in the Atomic Orbital
 ! g_a    : Atomic Orbital exponent      
 ! r      : Distance between the Atomic Orbital center and the considered point
 double precision, external :: ylm_orb
 integer :: n_a(3)
 double precision :: a(3), c(3), g_a
 integer :: i,j,k,l,m,n,p
 double precision :: dr, Ulc
 double precision :: y
 double precision, allocatable :: r(:)

 allocate (r(pseudo_grid_size))

 dr = pseudo_grid_rmax/dble(pseudo_grid_size)
 r(1) = 0.d0
 do j=2,pseudo_grid_size
   r(j) = r(j-1) + dr
 enddo

 mo_pseudo_grid = 0.d0
 do n=1,pseudo_grid_size
   do k=1,nucl_num
     do l=0,pseudo_lmax
       do m=-l,l
         do i=1,ao_num
           do j=1,mo_tot_num
             if (dabs(ao_pseudo_grid(i,m,l,k,n)) < 1.e-12) then
              cycle
             endif
             if (dabs(mo_coef(i,j)) < 1.e-8) then
              cycle
             endif
             mo_pseudo_grid(j,m,l,k,n) = mo_pseudo_grid(j,m,l,k,n) + &
                ao_pseudo_grid(i,m,l,k,n)  * mo_coef(i,j)
           enddo
         enddo
       enddo
     enddo
   enddo
 enddo
 deallocate(r)

END_PROVIDER

double precision function test_pseudo_grid_ao(i,j)
  implicit none
  integer, intent(in) :: i,j
  integer :: k,l,m,n
  double precision :: r, dr,u
  dr = pseudo_grid_rmax/dble(pseudo_grid_size)

  test_pseudo_grid_ao = 0.d0
  r = 0.d0
  do k=1,pseudo_grid_size
    do n=1,nucl_num
      do l = 0,pseudo_lmax
        u = pseudo_v_kl(n,l,1) * exp(-pseudo_dz_kl(n,l,1)*r*r)* r*r*dr
        do m=-l,l
          test_pseudo_grid_ao +=  ao_pseudo_grid(i,m,l,n,k) * ao_pseudo_grid(j,m,l,n,k) * u 
        enddo
      enddo
    enddo
    r = r+dr
  enddo
end
