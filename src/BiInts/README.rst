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

`ao_bielec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L1>`_
  integral of the AO basis <ik|jl> or (ij|kl)
  i(r1) j(r1) 1/r12 k(r2) l(r2)

`ao_bielec_integral_schwartz <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L326>`_
  Needed to compuet Schwartz inequalities

`ao_bielec_integrals_in_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L189>`_
  Map of Atomic integrals
  i(r1) j(r2) 1/r12 k(r1) l(r2)

`compute_ao_bielec_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L148>`_
  Compute AO 1/r12 integrals for all i and fixed j,k,l

`eri <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L487>`_
  ATOMIC PRIMTIVE bielectronic integral between the 4 primitives ::
  primitive_1 = x1**(a_x) y1**(a_y) z1**(a_z) exp(-alpha * r1**2)
  primitive_2 = x1**(b_x) y1**(b_y) z1**(b_z) exp(- beta * r1**2)
  primitive_3 = x2**(c_x) y2**(c_y) z2**(c_z) exp(-delta * r2**2)
  primitive_4 = x2**(d_x) y2**(d_y) z2**(d_z) exp(- gama * r2**2)

`general_primitive_integral <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L352>`_
  Computes the integral <pq|rs> where p,q,r,s are Gaussian primitives

`give_polynom_mult_center_x <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L632>`_
  subroutine that returns the explicit polynom in term of the "t"
  variable of the following polynomw :
  I_x1(a_x, d_x,p,q) * I_x1(a_y, d_y,p,q) * I_x1(a_z, d_z,p,q)

`i_x1_new <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L576>`_
  recursive function involved in the bielectronic integral

`i_x1_pol_mult <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L695>`_
  recursive function involved in the bielectronic integral

`i_x1_pol_mult_a1 <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L815>`_
  recursive function involved in the bielectronic integral

`i_x1_pol_mult_a2 <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L869>`_
  recursive function involved in the bielectronic integral

`i_x1_pol_mult_recurs <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L729>`_
  recursive function involved in the bielectronic integral

`i_x2_new <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L599>`_
  recursive function involved in the bielectronic integral

`i_x2_pol_mult <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L931>`_
  recursive function involved in the bielectronic integral

`integrale_new <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L531>`_
  calculate the integral of the polynom ::
  I_x1(a_x+b_x, c_x+d_x,p,q) * I_x1(a_y+b_y, c_y+d_y,p,q) * I_x1(a_z+b_z, c_z+d_z,p,q)
  between ( 0 ; 1)

`n_pt_sup <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/ao_bi_integrals.irp.f#L618>`_
  Returns the upper boundary of the degree of the polynom involved in the
  bielctronic integral :
  Ix(a_x,b_x,c_x,d_x) * Iy(a_y,b_y,c_y,d_y) * Iz(a_z,b_z,c_z,d_z)

`gauleg <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/gauss_legendre.irp.f#L20>`_
  Gauss-Legendre

`gauleg_t2 <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/gauss_legendre.irp.f#L1>`_
  t_w(i,1,k) = w(i)
  t_w(i,2,k) = t(i)

`gauleg_w <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/gauss_legendre.irp.f#L2>`_
  t_w(i,1,k) = w(i)
  t_w(i,2,k) = t(i)

`ao_integrals_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L6>`_
  AO integrals

`bielec_integrals_index <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L17>`_
None
`clear_ao_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L128>`_
  Frees the memory of the AO map

`clear_mo_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L243>`_
  Frees the memory of the MO map

`get_ao_bielec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L33>`_
  Gets one AO bi-electronic integral from the AO map

`get_ao_bielec_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L51>`_
  Gets multiple AO bi-electronic integral from the AO map .
  All i are retrieved for j,k,l fixed.

`get_ao_bielec_integrals_non_zero <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L84>`_
  Gets multiple AO bi-electronic integral from the AO map .
  All non-zero i are retrieved for j,k,l fixed.

`get_ao_map_size <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L120>`_
  Returns the number of elements in the AO map

`get_mo_bielec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L184>`_
  Returns one integral <ij|kl> in the MO basis

`get_mo_bielec_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L213>`_
  Returns multiple integrals <ij|kl> in the MO basis, all
  i for j,k,l fixed.

`get_mo_map_size <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L235>`_
  Return the number of elements in the MO map

`insert_into_ao_integrals_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L153>`_
  Create new entry into AO map

`insert_into_mo_integrals_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L168>`_
  Create new entry into MO map, or accumulate in an existing entry

`mo_bielec_integral <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L201>`_
  Returns one integral <ij|kl> in the MO basis

`mo_integrals_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/map_integrals.irp.f#L142>`_
  MO integrals

`add_integrals_to_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/mo_bi_integrals.irp.f#L40>`_
  Adds integrals to tha MO map according to some bitmask

`mo_bielec_integral_jj <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/mo_bi_integrals.irp.f#L296>`_
  Transform Bi-electronic integrals <ij|ij> and <ij|ji>

`mo_bielec_integral_jj_anti <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/mo_bi_integrals.irp.f#L298>`_
  Transform Bi-electronic integrals <ij|ij> and <ij|ji>

`mo_bielec_integral_jj_exchange <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/mo_bi_integrals.irp.f#L297>`_
  Transform Bi-electronic integrals <ij|ij> and <ij|ji>

`mo_bielec_integrals_in_map <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/mo_bi_integrals.irp.f#L21>`_
  If True, the map of MO bielectronic integrals is provided

`mo_bielec_integrals_index <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/mo_bi_integrals.irp.f#L1>`_
  Computes an unique index for i,j,k,l integrals

`ao_integrals_threshold <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/options.irp.f#L73>`_
  If <pq|rs> < ao_integrals_threshold, <pq|rs> = 0

`do_direct_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/options.irp.f#L111>`_
  If True, compute integrals on the fly

`mo_integrals_threshold <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/options.irp.f#L92>`_
  If <ij|kl> < mo_integrals_threshold, <ij|kl> = 0

`read_ao_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/options.irp.f#L55>`_
  If true, read AO integrals in EZFIO

`read_mo_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/options.irp.f#L37>`_
  If true, read MO integrals in EZFIO

`write_ao_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/options.irp.f#L19>`_
  If true, write AO integrals in EZFIO

`write_mo_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts/options.irp.f#L1>`_
  If true, write MO integrals in EZFIO



