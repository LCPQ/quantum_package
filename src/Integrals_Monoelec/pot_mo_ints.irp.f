BEGIN_PROVIDER [double precision, mo_nucl_elec_integral, (mo_tot_num_align,mo_tot_num)]
 implicit none
 integer :: i1,j1,i,j
 double precision :: c_i1,c_j1
 BEGIN_DOC
! interaction nuclear electron on the MO basis
 END_DOC

 double precision, allocatable :: X(:,:)
 allocate(X(ao_num_align,mo_tot_num))

 call dgemm('N','N',ao_num,mo_tot_num,ao_num,                        &
     1.d0,                                                           &
     ao_nucl_elec_integral, size(ao_nucl_elec_integral,1),           &
     mo_coef,size(mo_coef,1),                                        &
     0.d0, X, size(X,1))

 call dgemm('T','N',mo_tot_num,mo_tot_num,ao_num,                    &
     1.d0,                                                           &
     mo_coef,size(mo_coef,1),                                        &
     X, size(X,1),                                                   &
     0.d0, mo_nucl_elec_integral, size(mo_nucl_elec_integral,1))

 deallocate(X)

END_PROVIDER


BEGIN_PROVIDER [double precision, mo_nucl_elec_integral_per_atom, (mo_tot_num_align,mo_tot_num,nucl_num)]
 implicit none
 integer :: i1,j1,i,j,k
 double precision :: c_i1,c_j1
 BEGIN_DOC
! mo_nucl_elec_integral_per_atom(i,j,k) = -<MO(i)|1/|r-Rk|MO(j)> 
! where Rk is the geometry of the kth atom
 END_DOC

 allocate(X(ao_num_align,mo_tot_num))
 double precision, allocatable  :: X(:,:)
 
 do k = 1, nucl_num
   
   call dgemm('N','N',ao_num,mo_tot_num,ao_num,                      &
       1.d0,                                                         &
       ao_nucl_elec_integral_per_atom, size(ao_nucl_elec_integral_per_atom,1),&
       mo_coef,size(mo_coef,1),                                      &
       0.d0, X, size(X,1))
   
   call dgemm('T','N',mo_tot_num,mo_tot_num,ao_num,                  &
       1.d0,                                                         &
       mo_coef,size(mo_coef,1),                                      &
       X, size(X,1),                                                 &
       0.d0, mo_nucl_elec_integral_per_atom(1,1,k), size(mo_nucl_elec_integral_per_atom,1))
   
 enddo

 deallocate(X)
END_PROVIDER

