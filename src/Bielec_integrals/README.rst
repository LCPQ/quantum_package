=============
BiInts Module
=============

Here, all bi-electronic integrals (:math:`1/r_{12}`) are computed. As they have
4 indices and many are zero, they are stored in a map, as defined in
``Utils/map_module.f90``.  To fetch an AO integral, use the
``get_ao_bielec_integral(i,j,k,l,ao_integrals_map)`` function, and to fetch and
MO integral, use ``get_mo_bielec_integral(i,j,k,l,mo_integrals_map)`` or
``mo_bielec_integral(i,j,k,l)``.


Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `Bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_
* `MonoInts <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`ao_bielec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L1>`_
  integral of the AO basis <ik|jl> or (ij|kl)
  i(r1) j(r1) 1/r12 k(r2) l(r2)

`ao_bielec_integral_schwartz <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L490>`_
  Needed to compute Schwartz inequalities

`ao_bielec_integral_schwartz_accel <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L107>`_
  integral of the AO basis <ik|jl> or (ij|kl)
  i(r1) j(r1) 1/r12 k(r2) l(r2)

`ao_bielec_integrals_in_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L322>`_
  Map of Atomic integrals
  i(r1) j(r2) 1/r12 k(r1) l(r2)

`ao_l4 <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L279>`_
  Computes the product of l values of i,j,k,and l

`compute_ao_bielec_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L290>`_
  Compute AO 1/r12 integrals for all i and fixed j,k,l

`eri <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L653>`_
  ATOMIC PRIMTIVE bielectronic integral between the 4 primitives ::
  primitive_1 = x1**(a_x) y1**(a_y) z1**(a_z) exp(-alpha * r1**2)
  primitive_2 = x1**(b_x) y1**(b_y) z1**(b_z) exp(- beta * r1**2)
  primitive_3 = x2**(c_x) y2**(c_y) z2**(c_z) exp(-delta * r2**2)
  primitive_4 = x2**(d_x) y2**(d_y) z2**(d_z) exp(- gama * r2**2)

`general_primitive_integral <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L515>`_
  Computes the integral <pq|rs> where p,q,r,s are Gaussian primitives

`give_polynom_mult_center_x <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L851>`_
  subroutine that returns the explicit polynom in term of the "t"
  variable of the following polynomw :
  I_x1(a_x, d_x,p,q) * I_x1(a_y, d_y,p,q) * I_x1(a_z, d_z,p,q)

`i_x1_new <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L772>`_
  recursive function involved in the bielectronic integral

`i_x1_pol_mult <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L914>`_
  recursive function involved in the bielectronic integral

`i_x1_pol_mult_a1 <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L1034>`_
  recursive function involved in the bielectronic integral

`i_x1_pol_mult_a2 <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L1088>`_
  recursive function involved in the bielectronic integral

`i_x1_pol_mult_recurs <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L948>`_
  recursive function involved in the bielectronic integral

`i_x2_new <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L807>`_
  recursive function involved in the bielectronic integral

`i_x2_pol_mult <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L1150>`_
  recursive function involved in the bielectronic integral

`integrale_new <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L698>`_
  calculate the integral of the polynom ::
  I_x1(a_x+b_x, c_x+d_x,p,q) * I_x1(a_y+b_y, c_y+d_y,p,q) * I_x1(a_z+b_z, c_z+d_z,p,q)
  between ( 0 ; 1)

`n_pt_sup <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/ao_bi_integrals.irp.f#L837>`_
  Returns the upper boundary of the degree of the polynomial involved in the
  bielctronic integral :
  Ix(a_x,b_x,c_x,d_x) * Iy(a_y,b_y,c_y,d_y) * Iz(a_z,b_z,c_z,d_z)

`gauleg <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/gauss_legendre.irp.f#L29>`_
  Gauss-Legendre

`gauleg_t2 <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/gauss_legendre.irp.f#L10>`_
  t_w(i,1,k) = w(i)
  t_w(i,2,k) = t(i)

`gauleg_w <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/gauss_legendre.irp.f#L11>`_
  t_w(i,1,k) = w(i)
  t_w(i,2,k) = t(i)

`n_pt_max_integrals_16 <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/gauss_legendre.irp.f#L1>`_
  Aligned n_pt_max_integrals

`ao_integrals_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L6>`_
  AO integrals

`bielec_integrals_index <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L19>`_
  Undocumented

`bielec_integrals_index_reverse <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L36>`_
  Undocumented

`clear_ao_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L223>`_
  Frees the memory of the AO map

`clear_mo_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L399>`_
  Frees the memory of the MO map

`get_ao_bielec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L113>`_
  Gets one AO bi-electronic integral from the AO map

`get_ao_bielec_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L137>`_
  Gets multiple AO bi-electronic integral from the AO map .
  All i are retrieved for j,k,l fixed.

`get_ao_bielec_integrals_non_zero <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L172>`_
  Gets multiple AO bi-electronic integral from the AO map .
  All non-zero i are retrieved for j,k,l fixed.

`get_ao_map_size <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L214>`_
  Returns the number of elements in the AO map

`get_mo_bielec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L281>`_
  Returns one integral <ij|kl> in the MO basis

`get_mo_bielec_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L310>`_
  Returns multiple integrals <ij|kl> in the MO basis, all
  i for j,k,l fixed.

`get_mo_bielec_integrals_existing_ik <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L341>`_
  Returns multiple integrals <ij|kl> in the MO basis, all
  i(1)j(1) 1/r12 k(2)l(2)
  i for j,k,l fixed.

`get_mo_map_size <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L391>`_
  Return the number of elements in the MO map

`insert_into_ao_integrals_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L250>`_
  Create new entry into AO map

`insert_into_mo_integrals_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L265>`_
  Create new entry into MO map, or accumulate in an existing entry

`mo_bielec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L298>`_
  Returns one integral <ij|kl> in the MO basis

`mo_integrals_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/map_integrals.irp.f#L237>`_
  MO integrals

`add_integrals_to_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L42>`_
  Adds integrals to tha MO map according to some bitmask

`mo_bielec_integral_jj <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L464>`_
  mo_bielec_integral_jj(i,j) = J_ij
  mo_bielec_integral_jj_exchange(i,j) = K_ij
  mo_bielec_integral_jj_anti(i,j) = J_ij - K_ij

`mo_bielec_integral_jj_anti <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L466>`_
  mo_bielec_integral_jj(i,j) = J_ij
  mo_bielec_integral_jj_exchange(i,j) = K_ij
  mo_bielec_integral_jj_anti(i,j) = J_ij - K_ij

`mo_bielec_integral_jj_anti_from_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L326>`_
  mo_bielec_integral_jj_from_ao(i,j) = J_ij
  mo_bielec_integral_jj_exchange_from_ao(i,j) = J_ij
  mo_bielec_integral_jj_anti_from_ao(i,j) = J_ij - K_ij

`mo_bielec_integral_jj_exchange <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L465>`_
  mo_bielec_integral_jj(i,j) = J_ij
  mo_bielec_integral_jj_exchange(i,j) = K_ij
  mo_bielec_integral_jj_anti(i,j) = J_ij - K_ij

`mo_bielec_integral_jj_exchange_from_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L325>`_
  mo_bielec_integral_jj_from_ao(i,j) = J_ij
  mo_bielec_integral_jj_exchange_from_ao(i,j) = J_ij
  mo_bielec_integral_jj_anti_from_ao(i,j) = J_ij - K_ij

`mo_bielec_integral_jj_from_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L324>`_
  mo_bielec_integral_jj_from_ao(i,j) = J_ij
  mo_bielec_integral_jj_exchange_from_ao(i,j) = J_ij
  mo_bielec_integral_jj_anti_from_ao(i,j) = J_ij - K_ij

`mo_bielec_integrals_in_map <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L22>`_
  If True, the map of MO bielectronic integrals is provided

`mo_bielec_integrals_index <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/mo_bi_integrals.irp.f#L1>`_
  Computes an unique index for i,j,k,l integrals

`read_ao_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/read_write.irp.f#L1>`_
  One level of abstraction for disk_access_ao_integrals and disk_access_mo_integrals

`read_mo_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/read_write.irp.f#L2>`_
  One level of abstraction for disk_access_ao_integrals and disk_access_mo_integrals

`write_ao_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/read_write.irp.f#L3>`_
  One level of abstraction for disk_access_ao_integrals and disk_access_mo_integrals

`write_mo_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals/read_write.irp.f#L4>`_
  One level of abstraction for disk_access_ao_integrals and disk_access_mo_integrals



