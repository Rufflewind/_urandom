// Definitions
// -----------

// definition of mutability
~m ∈ {const, mut}

// promote const to trait bound => sharable
// promote mut   to trait bound => no constraint
mut + const = const
const: mut

// Laws of references
// ------------------

// well-formedness
⊢ &'a ~m T: 'a

// type covariance
&'a ~m T → &'a ~m U
  where U: T

// lifetime covariance
&'a ~m T → &'b ~m T
  where 'a: 'b

// mutability covariance
&'a ~m T → &'a const T

// reborrowing
&'b ~m1 &'a ~m2 T → &'b ~(m1 + m2) T

// immutable clonability
&'b ~m &'a const T → &'a const T
