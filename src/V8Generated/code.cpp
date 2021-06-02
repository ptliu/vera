
#define V8_INFINITY ((double)9218868437227405312)
#define kMaxInt ((int32_t) 2147483647)
#define kMinInt ((int32_t) -kMaxInt - (int32_t)1)
#define kMaxUInt32 ((uint32_t) 4294967295)

#define bitset_t uint32_t

// define internal bitset types
#define kOtherUnsigned31     (uint32_t)1 << (uint32_t)1
#define kOtherUnsigned32     (uint32_t)1 << (uint32_t)2
#define kOtherSigned32       (uint32_t)1 << (uint32_t)3
#define kOtherNumber         (uint32_t)1 << (uint32_t)4
#define kOtherString         (uint32_t)1 << (uint32_t)5

// define external bitset types
#define kNone                     (uint32_t)0
#define kNegative31               (uint32_t)1 << (uint32_t)6
#define kNull                     (uint32_t)1 << (uint32_t)7
#define kUndefined                (uint32_t)1 << (uint32_t)8
#define kBoolean                  (uint32_t)1 << (uint32_t)9
#define kUnsigned30               (uint32_t)1 << (uint32_t)10
#define kMinusZero                (uint32_t)1 << (uint32_t)11
#define kNaN                      (uint32_t)1 << (uint32_t)12
#define kSymbol                   (uint32_t)1 << (uint32_t)13
#define kInternalizedString       (uint32_t)1 << (uint32_t)14
#define kOtherCallable            (uint32_t)1 << (uint32_t)16
#define kOtherObject              (uint32_t)1 << (uint32_t)17
#define kOtherUndetectable        (uint32_t)1 << (uint32_t)18
#define kCallableProxy            (uint32_t)1 << (uint32_t)19
#define kOtherProxy               (uint32_t)1 << (uint32_t)20
#define kFunction                 (uint32_t)1 << (uint32_t)21
#define kBoundFunction            (uint32_t)1 << (uint32_t)22
#define kHole                     (uint32_t)1 << (uint32_t)23
#define kOtherInternal            (uint32_t)1 << (uint32_t)24
#define kExternalPointer          (uint32_t)1 << (uint32_t)25
#define kArray                    (uint32_t)1 << (uint32_t)26
#define kBigInt                   (uint32_t)1 << (uint32_t)27
  /* TODO(v8:10391): Remove this type once all ExternalPointer usages are */ \
  /* sandbox-ready. */                   \
#define kSandboxedExternalPointer (uint32_t)1 << (uint32_t)28
#define kSigned31                     kUnsigned30 | kNegative31
#define kSigned32                     kSigned31 | kOtherUnsigned31 | \
                                  kOtherSigned32
#define kSigned32OrMinusZero          kSigned32 | kMinusZero
#define kSigned32OrMinusZeroOrNaN     kSigned32 | kMinusZero | kNaN
#define kNegative32                   kNegative31 | kOtherSigned32
#define kUnsigned31                   kUnsigned30 | kOtherUnsigned31
#define kUnsigned32                   kUnsigned30 | kOtherUnsigned31 | \
                                  kOtherUnsigned32
#define kUnsigned32OrMinusZero        kUnsigned32 | kMinusZero
#define kUnsigned32OrMinusZeroOrNaN   kUnsigned32 | kMinusZero | kNaN
#define kIntegral32                   kSigned32 | kUnsigned32
#define kIntegral32OrMinusZero        kIntegral32 | kMinusZero
#define kIntegral32OrMinusZeroOrNaN   kIntegral32OrMinusZero | kNaN
#define kPlainNumber                  kIntegral32 | kOtherNumber
#define kOrderedNumber                kPlainNumber | kMinusZero
#define kMinusZeroOrNaN               kMinusZero | kNaN
#define kNumber                       kOrderedNumber | kNaN
#define kNumeric                      kNumber | kBigInt
#define kString                       kInternalizedString | kOtherString
#define kUniqueName                   kSymbol | kInternalizedString
#define kName                         kSymbol | kString
#define kInternalizedStringOrNull     kInternalizedString | kNull
#define kBooleanOrNumber              kBoolean | kNumber
#define kBooleanOrNullOrNumber        kBooleanOrNumber | kNull
#define kBooleanOrNullOrUndefined     kBoolean | kNull | kUndefined
#define kOddball                      kBooleanOrNullOrUndefined | kHole
#define kNullOrNumber                 kNull | kNumber
#define kNullOrUndefined              kNull | kUndefined
#define kUndetectable                 kNullOrUndefined | kOtherUndetectable
#define kNumberOrHole                 kNumber | kHole
#define kNumberOrOddball              kNumber | kNullOrUndefined | kBoolean | \
                                     kHole
#define kNumericOrString              kNumeric | kString
#define kNumberOrUndefined            kNumber | kUndefined
#define kNumberOrUndefinedOrNullOrBoolean  \
                                     kNumber | kNullOrUndefined | kBoolean
#define kPlainPrimitive               kNumber | kString | kBoolean | \
                                     kNullOrUndefined
#define kNonBigIntPrimitive           kSymbol | kPlainPrimitive
#define kPrimitive                    kBigInt | kNonBigIntPrimitive
#define kOtherUndetectableOrUndefined kOtherUndetectable | kUndefined
#define kProxy                        kCallableProxy | kOtherProxy
#define kArrayOrOtherObject           kArray | kOtherObject
#define kArrayOrProxy                 kArray | kProxy
#define kDetectableCallable           kFunction | kBoundFunction | \
                                     kOtherCallable | kCallableProxy
#define kCallable                     kDetectableCallable | kOtherUndetectable
#define kNonCallable                  kArray | kOtherObject | kOtherProxy
#define kNonCallableOrNull            kNonCallable | kNull
#define kDetectableObject             kArray | kFunction | kBoundFunction | \
                                     kOtherCallable | kOtherObject
#define kDetectableReceiver           kDetectableObject | kProxy
#define kDetectableReceiverOrNull     kDetectableReceiver | kNull
#define kObject                       kDetectableObject | kOtherUndetectable
#define kReceiver                     kObject | kProxy
#define kReceiverOrUndefined          kReceiver | kUndefined
#define kReceiverOrNullOrUndefined    kReceiver | kNull | kUndefined
#define kSymbolOrReceiver             kSymbol | kReceiver
#define kStringOrReceiver             kString | kReceiver
#define kUnique                       kBoolean | kUniqueName | kNull | \
                                     kUndefined | kHole | kReceiver
#define kInternal                     kHole | kExternalPointer | \
                                     kSandboxedExternalPointer | kOtherInternal
#define kNonInternal                  kPrimitive | kReceiver
#define kNonBigInt                    kNonBigIntPrimitive | kReceiver
#define kNonNumber                    kBigInt | kUnique | kString | kInternal
#define kAny                          (uint32_t)0xfffffffe

#define UINT32_ONE (uint32_t)1
#define UINT32_ZERO (uint32_t)0
#define FALSE (bool)0
#define TRUE (bool)1 //be careful with this one, don't compare directly against it
#define DOUBLE_ZERO (double)0
#define DOUBLE_ONE (double)1

#define kSingletonZero newRange(DOUBLE_ZERO, DOUBLE_ZERO)
#define kInteger newRange(minusInfinityType(), infinityType())

struct limits {
    double min;
    double max;
};

// Style TODOs:
// 1. const blah& not allowed, only blah const&
// 2. ++blah not allowed
// 3. inner structs not allowed
// 4. No classes, so no methods
// 5. Doesn't support boundaries array or anything, just
//  encode as method...
//
// Wacky TODOs:
// 1. BitsetType -> just the bitset
// 2. TypeBase -> just superclass for non-biset types
// 3. RangeType -> has a bisettype and range limits
// 4. Type class
//      - Has internal bitset type
//      - has payload_
//          - if payload lsb is 1, is bitset
//          - if payload lsb is 0, is ptr to subclass of TypeBase type (i.e. range or union)
//      - has bunch of methods
//          - Is()
//          - IsBitset()
//          - yada yada

// oversimplifed V8 type
//  - Consolidated all the type class hierarchy into a single type
//  - Some weird stuff that isn't here at all
//  - no "OtherNumberConstantType"
//  - no "HeapConstantType"
//  - no "TypeType"
//  - no "UnionType"
//  - flat, cause nested structs not supported
//
// unions: return any bitset, with maybeNan, maybeMinusZero
// TODO: Cannot have "limits" inner struct right now, would need to extend dsl
//  probably not worth doing, doesn't add extra meaning
//  Current soln is to just manually set fields from limits struct .... gross
struct v8type {
    bitset_t bitset;
    bool hasRange;
    double min;
    double max;
    bool maybeNaN;
    bool maybeMinusZero;
    bool isUnion;
};

struct boundary {
    bitset_t internal;
    bitset_t external;
    double min;
};

// NOTES:
// 1. MUST ANNOTATE MAGIC NUMBERS WITH TYPES OR WILL REE AT U

double test(double test_input) {
    double test_yeet = test_input;
    return 0.0;
}

double test2(double test2_input) {
    double test2_yeet = 0.0;
    test(1.0);
    return test2_yeet;
}

// Helpers
double min4(double min4_one, double min4_two, double min4_three, double min4_four) {
    double min4_min = min4_one;
    if (min4_two < min4_min) {
        min4_min = min4_two;
    }
    if (min4_three < min4_min) {
        min4_min = min4_three;
    } 
    if (min4_four < min4_min) {
        min4_min = min4_four;
    }

    return min4_min;
}

double max4(double max4_one, double max4_two, double max4_three, double max4_four) {
    double max4_max = max4_one;
    if (max4_two > max4_max) {
        max4_max = max4_two;
    }
    if (max4_three > max4_max) {
        max4_max = max4_three;
    } 
    if (max4_four > max4_max) {
        max4_max = max4_four;
    }

    return max4_max;
}

v8type newBitset(bitset_t newbitset_bits) {
    v8type newbitset_type;
    newbitset_type.bitset = newbitset_bits;
    newbitset_type.hasRange = (bool)1;
    newbitset_type.max = DOUBLE_ZERO;
    newbitset_type.min = DOUBLE_ZERO;
    newbitset_type.maybeNaN = (bool)0;
    newbitset_type.maybeMinusZero = (bool)0;
    newbitset_type.isUnion = (bool)0;
    return newbitset_type;
}

v8type newRange(double newrange_min, double newrange_max) {
    v8type newrange_type;
    newrange_type.bitset = BitsetTypeLub(newrange_min, newrange_max);
    newrange_type.hasRange = (bool)1;
    newrange_type.max = newrange_max;
    newrange_type.min = newrange_min;
    newrange_type.maybeNaN = (bool)0;
    newrange_type.maybeMinusZero = (bool)0;
    newrange_type.isUnion = (bool)0;
    return newrange_type;
}

v8type nanType() {
    v8type nantype_type;
    nantype_type.bitset = kNaN;
    nantype_type.hasRange = FALSE;
    nantype_type.max = DOUBLE_ZERO;
    nantype_type.min = DOUBLE_ZERO;
    nantype_type.maybeNaN = TRUE;
    nantype_type.maybeMinusZero = FALSE;
    nantype_type.isUnion = FALSE;
    return nantype_type;
}

v8type noneType() {
    v8type nonetype_type;
    nonetype_type.bitset = kNone;
    nonetype_type.hasRange = FALSE;
    nonetype_type.max = DOUBLE_ZERO;
    nonetype_type.min = DOUBLE_ZERO;
    nonetype_type.maybeNaN = FALSE;
    nonetype_type.maybeMinusZero = FALSE;
    nonetype_type.isUnion = FALSE;
    return nonetype_type;
}

v8type minusZeroType() {
    v8type minuszerotype_type;
    minuszerotype_type.bitset = kMinusZero;
    minuszerotype_type.hasRange = FALSE;
    minuszerotype_type.max = DOUBLE_ZERO;
    minuszerotype_type.min = DOUBLE_ZERO;
    minuszerotype_type.maybeNaN = FALSE;
    minuszerotype_type.maybeMinusZero = TRUE;
    minuszerotype_type.isUnion = FALSE;
    return minuszerotype_type;
}

v8type AnyType() {
    v8type anytype_type;
    anytype_type.bitset = kAny;
    anytype_type.hasRange = FALSE;
    anytype_type.max = DOUBLE_ZERO;
    anytype_type.min = DOUBLE_ZERO;
    anytype_type.maybeNaN = TRUE;
    anytype_type.maybeMinusZero = TRUE;
    anytype_type.isUnion = FALSE;
    return anytype_type;
}

v8type minusInfinityType() {
    v8type minusinfinitytype_type;
    minusinfinitytype_type.bitset = kOtherNumber;
    minusinfinitytype_type.max = -V8_INFINITY;
    minusinfinitytype_type.min = -V8_INFINITY;
    minusinfinitytype_type.hasRange = TRUE; 
    minusinfinitytype_type.maybeNaN = FALSE;
    minusinfinitytype_type.maybeMinusZero = FALSE;
    minusinfinitytype_type.isUnion = FALSE;
}

v8type infinityType() {
    v8type infinitytype_type;
    infinitytype_type.bitset = kOtherNumber;
    infinitytype_type.max = V8_INFINITY;
    infinitytype_type.min = V8_INFINITY;
    infinitytype_type.hasRange = TRUE; 
    infinitytype_type.maybeNaN = FALSE;
    infinitytype_type.maybeMinusZero = FALSE;
    infinitytype_type.isUnion = FALSE;
}

v8type plainNumberType() { 
    v8type plainnumbertype_type;
    plainnumbertype_type.bitset = kPlainNumber;
    plainnumbertype_type.hasRange = FALSE; 
    plainnumbertype_type.maybeNaN = FALSE;
    plainnumbertype_type.maybeMinusZero = FALSE;
    plainnumbertype_type.isUnion = FALSE;
}

limits copy(limits const& copy_other) {
    limits copy_lresult;
    copy_lresult.min = copy_other.min;
    copy_lresult.max = copy_other.max;
    return copy_lresult;
}

limits getLimits(v8type const& getlimits_ty) {
    limits getlimits_lresult;
    getlimits_lresult.min = getlimits_ty.min; // TODO: wrong!
    getlimits_lresult.max = getlimits_ty.max;
    return getlimits_lresult;
}

// boundary helpers

boundary getBoundary(uint32_t getboundary_index) {
    boundary getboundary_bound;
    getboundary_bound.internal = kOtherNumber;
    getboundary_bound.external = kPlainNumber;
    getboundary_bound.min = 0.0;

    if (getboundary_index == (uint32_t)0) {
        getboundary_bound.internal = kOtherNumber;
        getboundary_bound.external = kPlainNumber;
        getboundary_bound.min = (double)-V8_INFINITY;
    }
    if (getboundary_index == (uint32_t)1) {
        getboundary_bound.internal = kOtherSigned32;
        getboundary_bound.external = kNegative32;
        getboundary_bound.min = (double)kMinInt;
    }
    if (getboundary_index == (uint32_t)2) {
        getboundary_bound.internal = kNegative31;
        getboundary_bound.external = kNegative31;
        getboundary_bound.min = (double)-1073741824;
    }
    if (getboundary_index == (uint32_t)3) {
        getboundary_bound.internal = kUnsigned30;
        getboundary_bound.external = kUnsigned30;
        getboundary_bound.min = (double)0;
    }
    if (getboundary_index == (uint32_t)4) {
        getboundary_bound.internal = kOtherUnsigned31;
        getboundary_bound.external = kUnsigned31;
        getboundary_bound.min = (double)1073741824;
    }
    if (getboundary_index == (uint32_t)5) {
        getboundary_bound.internal = kOtherUnsigned32;
        getboundary_bound.external = kUnsigned32;
        getboundary_bound.min = (double)2147483648;
    }
    if (getboundary_index == (uint32_t)6) {
        getboundary_bound.internal = kOtherNumber;
        getboundary_bound.external = kPlainNumber;
        getboundary_bound.min = (double)kMaxUInt32 + (double)1;
    }

    return getboundary_bound;
}

uint32_t BoundariesSize() {
    return (uint32_t)7;
}

//bit helper functions
bool SignedAddWouldOverflow32(int32_t signedaddaouldaverflow32_ulhs, int32_t signedaddaouldaverflow32_urhs){
  uint32_t signedaddaouldaverflow32_res = (uint32_t)signedaddaouldaverflow32_ulhs + (uint32_t)signedaddaouldaverflow32_urhs;
  return ((signedaddaouldaverflow32_res ^ signedaddaouldaverflow32_ulhs) & (signedaddaouldaverflow32_res ^ signedaddaouldaverflow32_urhs) & ((uint32_t) 1 << (uint32_t)31)) != (uint32_t)0;
}

// Range-related helper functions
//

// must use lthis_ because this_ would interfere with v8type funcs, as param declarations are shared...
// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=22
bool IsEmpty(limits isempty_lthis_) { 
    return isempty_lthis_.min > isempty_lthis_.max;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=24
limits LimitIntersect(limits const& limitintersect_llhs, limits const& limitintersect_lrhs) {
  //DisallowGarbageCollection no_gc;
  limits limitintersect_lresult;
  limitintersect_lresult = copy(limitintersect_llhs);

  if (limitintersect_llhs.min < limitintersect_lrhs.min) {
      limitintersect_lresult.min = limitintersect_lrhs.min;
  }
  if (limitintersect_llhs.max > limitintersect_lrhs.max) { 
      limitintersect_lresult.max = limitintersect_lrhs.max;
  }
  return limitintersect_lresult;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=32
limits Union(limits const& union_llhs, limits const& union_lrhs) {
  //DisallowGarbageCollection no_gc;
  limits union_lresult;
  if (IsEmpty(union_llhs)) {
      return union_lrhs;
  }
  if (IsEmpty(union_lrhs)) {
      return union_llhs;
  }
  union_lresult = copy(union_llhs);
  if (union_llhs.min > union_lrhs.min) {
    union_lresult.min = union_lrhs.min;
  }
  if (union_llhs.max < union_lrhs.max) {
      union_lresult.max = union_lrhs.max;
  }
  return union_lresult;
}


// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=42
bool Overlap(v8type const& overlap_lhs, v8type const& overlap_rhs) {
  //DisallowGarbageCollection no_gc;
  return !IsEmpty(LimitIntersect(getLimits(overlap_lhs),
                    getLimits(overlap_rhs)));
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=49
// TODO
/*bool Type::Contains(const v8type& lhs, const v8type& rhs) {
  //DisallowGarbageCollection no_gc;
  return lhs->Min() <= rhs->Min() && rhs->Max() <= lhs->Max();
}*/

// Oversimplified IsBitset. Can't do original cause we don't have
// type hierarchy
bool IsBitset(v8type const& isbitset_this_) {
    // TODO:
    return !(isbitset_this_.hasRange || isbitset_this_.isUnion); 
}

bool IsRange(v8type const& isrange_this_) {
    return isrange_this_.hasRange && !isrange_this_.isUnion; 
}

bool IsUnion(v8type const& isunion_this_) {
    return isunion_this_.isUnion;
}

bool IsTuple(v8type const& istuple_this_) {
    return FALSE;
}

bool TypeIsNone(v8type typeisnone_ty){
  return BitsetIsNone(typeisnone_ty.bitset);
}

bool TypeIsAny(v8type typeisany_ty) {
    return typeisany_ty.bitset == kAny;
}

// Bitset methods

bool BitsetIsNone(bitset_t bitsetisnone_bits) {
    return bitsetisnone_bits == kNone;
}

bool BitsetIs(bitset_t bitsetis_bits1, bitset_t bitsetis_bits2) {
    return (bitsetis_bits1 | bitsetis_bits2) == bitsetis_bits2;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=438
double BitsetMin(bitset_t bitsetmin_bits) {
  //DisallowGarbageCollection no_gc;
  //DCHECK(Is(bits, kNumber)); // precond
  //DCHECK(!Is(bits, kNaN)); // precond
  //const Boundary* mins = Boundaries();
  
  bool bitsetmin_mz = ((bitsetmin_bits & kMinusZero) != (bitset_t)0);

  // DELEGATED TO minBoundary helper, cause we dont got loops...
  // unroll the loop
  boundary bitsetmin_minBound = getBoundary((uint32_t)0);
  if (BitsetIs(bitsetmin_minBound.internal, bitsetmin_bits)) {
    return bitsetmin_mz ? math::min((double)0, bitsetmin_minBound.min) : bitsetmin_minBound.min;
  }
  boundary bitsetmin_minBound = getBoundary((uint32_t)1);
  if (BitsetIs(bitsetmin_minBound.internal, bitsetmin_bits)) {
    return bitsetmin_mz ? math::min((double)0, bitsetmin_minBound.min) : bitsetmin_minBound.min;
  }
  boundary bitsetmin_minBound = getBoundary((uint32_t)2);
  if (BitsetIs(bitsetmin_minBound.internal, bitsetmin_bits)) {
    return bitsetmin_mz ? math::min((double)0, bitsetmin_minBound.min) : bitsetmin_minBound.min;
  }
  boundary bitsetmin_minBound = getBoundary((uint32_t)3);
  if (BitsetIs(bitsetmin_minBound.internal, bitsetmin_bits)) {
    return bitsetmin_mz ? math::min((double)0, bitsetmin_minBound.min) : bitsetmin_minBound.min;
  }
  boundary bitsetmin_minBound = getBoundary((uint32_t)4);
  if (BitsetIs(bitsetmin_minBound.internal, bitsetmin_bits)) {
    return bitsetmin_mz ? math::min((double)0, bitsetmin_minBound.min) : bitsetmin_minBound.min;
  }
  boundary bitsetmin_minBound = getBoundary((uint32_t)5);
  if (BitsetIs(bitsetmin_minBound.internal, bitsetmin_bits)) {
    return bitsetmin_mz ? math::min((double)0, bitsetmin_minBound.min) : bitsetmin_minBound.min;
  }
  boundary bitsetmin_minBound = getBoundary((uint32_t)6);
  if (BitsetIs(bitsetmin_minBound.internal, bitsetmin_bits)) {
    return bitsetmin_mz ? math::min((double)0, bitsetmin_minBound.min) : bitsetmin_minBound.min;
  }

  // TODO: this fails cause mz
  //DCHECK(mz);
  return 0.0;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=453;bpv=0;bpt=1
double BitsetMax(bitset_t bitsetmax_bits) {
  //DisallowGarbageCollection no_gc;
  //DCHECK(Is(bits, kNumber));
  //DCHECK(!Is(bits, kNaN));
  
  bool bitsetmax_mz = bitsetmax_bits & kMinusZero;
  if (BitsetIs(getBoundary((uint32_t)BoundariesSize() - UINT32_ONE).internal, bitsetmax_bits)) {
    return V8_INFINITY;
  }

  // unrolled
  /*for (size_t i = BoundariesSize() - 1; i-- > 0;) {
    if (Is(mins[i].internal, bits)) {
      return mz ? std::max(0.0, mins[i + 1].min - 1) : mins[i + 1].min - 1;
    }
  }*/
  uint32_t i = (uint32_t)BoundariesSize() - UINT32_ONE;
  boundary bitsetmax_min = getBoundary(i);
  if (BitsetIs(bitsetmax_min.internal, bitsetmax_bits)) {
    boundary bitsetmax_min1 = getBoundary(i+UINT32_ONE);
    return bitsetmax_mz ? math::min((double)0, bitsetmax_min1.min - UINT32_ONE) : bitsetmax_min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary bitsetmax_min = getBoundary(i);
  if (BitsetIs(bitsetmax_min.internal, bitsetmax_bits)) {
    boundary bitsetmax_min1 = getBoundary(i+UINT32_ONE);
    return bitsetmax_mz ? math::min((double)0, bitsetmax_min1.min - UINT32_ONE) : bitsetmax_min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary bitsetmax_min = getBoundary(i);
  if (BitsetIs(bitsetmax_min.internal, bitsetmax_bits)) {
    boundary bitsetmax_min1 = getBoundary(i+UINT32_ONE);
    return bitsetmax_mz ? math::min((double)0, bitsetmax_min1.min - UINT32_ONE) : bitsetmax_min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary bitsetmax_min = getBoundary(i);
  if (BitsetIs(bitsetmax_min.internal, bitsetmax_bits)) {
    boundary bitsetmax_min1 = getBoundary(i+UINT32_ONE);
    return bitsetmax_mz ? math::min((double)0, bitsetmax_min1.min - UINT32_ONE) : bitsetmax_min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary bitsetmax_min = getBoundary(i);
  if (BitsetIs(bitsetmax_min.internal, bitsetmax_bits)) {
    boundary bitsetmax_min1 = getBoundary(i+UINT32_ONE);
    return bitsetmax_mz ? math::min((double)0, bitsetmax_min1.min - UINT32_ONE) : bitsetmax_min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary bitsetmax_min = getBoundary(i);
  if (BitsetIs(bitsetmax_min.internal, bitsetmax_bits)) {
    boundary bitsetmax_min1 = getBoundary(i+UINT32_ONE);
    return bitsetmax_mz ? math::min((double)0, bitsetmax_min1.min - UINT32_ONE) : bitsetmax_min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary bitsetmax_min = getBoundary(i);
  if (BitsetIs(bitsetmax_min.internal, bitsetmax_bits)) {
    boundary bitsetmax_min1 = getBoundary(i+UINT32_ONE);
    return bitsetmax_mz ? math::min((double)0, bitsetmax_min1.min - UINT32_ONE) : bitsetmax_min1.min - UINT32_ONE;
  }

  return DOUBLE_ZERO;
}

bitset_t NumberBits(bitset_t numberbits_bits) {
    return numberbits_bits & kPlainNumber;
}

bitset_t BitsetTypeLub(double bitsettypelub_min, double bitsettypelub_max) {
//Type::bitset BitsetType::Lub(double min, double max) {
  //DisallowGarbageCollection no_gc;
  //int lub = kNone;
  bitset_t lub = kNone;
  //const Boundary* mins = Boundaries();

  // unrolled...
  /*for (size_t i = 1; i < BoundariesSize(); ++i) {
    if (min < mins[i].min) {
      lub |= mins[i - 1].internal;
      if (max < mins[i].min) return lub;
    }
  }*/
  boundary bitsettypelub_mins_1 = getBoundary((uint32_t)1);
  if (bitsettypelub_min < bitsettypelub_mins_1.min) {
      lub |= bitsettypelub_mins_1.internal;
      if (bitsettypelub_max < bitsettypelub_mins_1.min) {
          return lub;
      }
  }
  boundary bitsettypelub_mins_2 = getBoundary((uint32_t)2);
  if (bitsettypelub_min < bitsettypelub_mins_2.min) {
      lub |= bitsettypelub_mins_2.internal;
      if (bitsettypelub_max < bitsettypelub_mins_2.min) {
          return lub;
      }
  }
  boundary bitsettypelub_mins_3 = getBoundary((uint32_t)3);
  if (bitsettypelub_min < bitsettypelub_mins_3.min) {
      lub |= bitsettypelub_mins_3.internal;
      if (bitsettypelub_max < bitsettypelub_mins_3.min) {
          return lub;
      }
  }
  boundary bitsettypelub_mins_4 = getBoundary((uint32_t)4);
  if (bitsettypelub_min < bitsettypelub_mins_4.min) {
      lub |= bitsettypelub_mins_4.internal;
      if (bitsettypelub_max < bitsettypelub_mins_4.min) {
          return lub;
      }
  }
  boundary bitsettypelub_mins_5 = getBoundary((uint32_t)5);
  if (bitsettypelub_min < bitsettypelub_mins_5.min) {
      lub |= bitsettypelub_mins_5.internal;
      if (bitsettypelub_max < bitsettypelub_mins_5.min) {
          return lub;
      }
  }
  boundary bitsettypelub_mins_6 = getBoundary((uint32_t)6);
  if (bitsettypelub_min < bitsettypelub_mins_6.min) {
      lub |= bitsettypelub_mins_6.internal;
      if (bitsettypelub_max < bitsettypelub_mins_6.min) {
          return lub;
      }
  }
  boundary bitsettypelub_mins_7 = getBoundary((uint32_t)7);
  if (bitsettypelub_min < bitsettypelub_mins_7.min) {
      lub |= bitsettypelub_mins_7.internal;
      if (bitsettypelub_max < bitsettypelub_mins_7.min) {
          return lub;
      }
  }

  // return lub | mins[BoundariesSize() - 1].internal;
  lub |= bitsettypelub_mins_7.internal;
  return lub;
}

// TODO: rename this...
// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=418
bitset_t BitsetTypeGlb(double bitsettypeglb_min, double bitsettypeglb_max) {
  bitset_t bitsettypeglb_glb = kNone; //NOTE: Type of glb changed from int(int is weird, why is it signed?)

  // DELEGATED TO minBoundary helper, cause we dont got loops...
  // unroll the loop
  bool bitsettypeglb_continueLoop = TRUE;
  if(bitsettypeglb_max <  (double)-1 || bitsettypeglb_min > DOUBLE_ZERO){
    return bitsettypeglb_glb;
  }

  boundary bitsettypeglb_mins_1 = getBoundary((uint32_t)1);
  
  boundary bitsettypeglb_mins_2 = getBoundary((uint32_t)2);
  if (bitsettypeglb_min < bitsettypeglb_mins_1.min && bitsettypeglb_continueLoop) {
    if(bitsettypeglb_max + DOUBLE_ONE < bitsettypeglb_mins_2.min){
      bitsettypeglb_continueLoop = FALSE;
    }
    bitsettypeglb_glb = bitsettypeglb_glb | bitsettypeglb_mins_1.external;
  }


  boundary bitsettypeglb_mins_3 = getBoundary((uint32_t)3);
  if (bitsettypeglb_min < bitsettypeglb_mins_2.min && bitsettypeglb_continueLoop) {
    if(bitsettypeglb_max + DOUBLE_ONE < bitsettypeglb_mins_3.min){
      bitsettypeglb_continueLoop = FALSE;
    }
    bitsettypeglb_glb = bitsettypeglb_glb | bitsettypeglb_mins_2.external;
  }

  boundary bitsettypeglb_mins_4 = getBoundary((uint32_t)4);
  if (bitsettypeglb_min < bitsettypeglb_mins_3.min && bitsettypeglb_continueLoop) {
    if(bitsettypeglb_max + DOUBLE_ONE < bitsettypeglb_mins_4.min){
      bitsettypeglb_continueLoop = FALSE;
    }
    bitsettypeglb_glb = bitsettypeglb_glb | bitsettypeglb_mins_3.external;
  }

  boundary bitsettypeglb_mins_5 = getBoundary((uint32_t)5);
  if (bitsettypeglb_min < bitsettypeglb_mins_4.min && bitsettypeglb_continueLoop) {
    if(bitsettypeglb_max + DOUBLE_ONE < bitsettypeglb_mins_5.min){
      bitsettypeglb_continueLoop = FALSE;
    }
    bitsettypeglb_glb = bitsettypeglb_glb | bitsettypeglb_mins_5.external;
  }

  boundary bitsettypeglb_mins_6 = getBoundary((uint32_t)6);
  if (bitsettypeglb_min < bitsettypeglb_mins_5.min && bitsettypeglb_continueLoop) {
    if(bitsettypeglb_max + DOUBLE_ONE < bitsettypeglb_mins_6.min){
      bitsettypeglb_continueLoop = FALSE;
    }
    bitsettypeglb_glb = bitsettypeglb_glb | bitsettypeglb_mins_5.external;
  }
  return bitsettypeglb_glb & ~(kOtherNumber);
}


// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=114
bitset_t BitsetLub(v8type bitsetlub_this_) {
  // The smallest bitset subsuming this type, possibly not a proper one.

  // DisallowGarbageCollection no_gc;
  bool bitsetlub_unreachable_ = (bool) 0; //TODO: assert that this is false to enforce
                                //code is unreachable at bottom

  if (IsBitset(bitsetlub_this_)) {
    return bitsetlub_this_.bitset;
  }

  if (IsUnion(bitsetlub_this_)) {
    //   // Take the representation from the first element, which is always
    //   // a bitset.
    //   int bitset = AsUnion()->Get(0).BitsetLub();
    //   for (int i = 0, n = AsUnion()->Length(); i < n; ++i) {
    //     // Other elements only contribute their semantic part.
    //     bitset |= AsUnion()->Get(i).BitsetLub();
    //   }
    //   return bitset;
    return bitsetlub_this_.bitset;
  }

  // if (IsHeapConstant()) return AsHeapConstant()->Lub();
  // if (IsOtherNumberConstant()) {
  //   return AsOtherNumberConstant()->Lub();
  // }

  if (IsRange(bitsetlub_this_)) {
    return bitsetlub_this_.bitset;
    // return AsRange()->Lub();
  }

  if (IsTuple(bitsetlub_this_)) {
    return kOtherInternal;
  }

  bitsetlub_unreachable_ = (bool) 1; //TODO: assert that this is false to enforce 
                           //that this line is never reached
  
  // UNREACHABLE();
}

//https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=96
bitset_t BitsetGlb(v8type bitsetglb_this_) {
  if(IsBitset(bitsetglb_this_)){
    return AsBitset(bitsetglb_this_);
  } else if (IsUnion(bitsetglb_this_)){
    //TODO don't handle unions yet
    return AsBitset(bitsetglb_this_);
  } else if(IsRange(bitsetglb_this_)){
    return BitsetTypeGlb(bitsetglb_this_.min, bitsetglb_this_.max);
  } else {
    return kNone;
  }
}


// Type methods

// ---
// Ctors
v8type NewBitset(bitset_t newbitset_bitset) {
    v8type newbitset_type;
    newbitset_type.bitset = newbitset_bitset;
    newbitset_type.hasRange = FALSE;
    newbitset_type.max = DOUBLE_ZERO;
    newbitset_type.min = DOUBLE_ZERO;
    newbitset_type.maybeNaN = FALSE;
    newbitset_type.maybeMinusZero = FALSE;
    newbitset_type.isUnion = FALSE;
    return newbitset_type;
}

// ----
// Casts

bitset_t AsBitset(v8type asbitset_this_) {
    return asbitset_this_.bitset;
}

// Minimum and maximum of a numeric type.
// These functions do not distinguish between -0 and +0.  NaN is ignored.
// Only call them on subtypes of Number whose intersection with OrderedNumber
// is not empty.
/*double Min(v8type const& this_) const {
  //DCHECK(this->Is(Number())); TODO
  //DCHECK(!this->Is(NaN()));
  if (IsBitset(this_)) {
      return BitsetMin(this_.bitset);
ion()) {
    double min = +V8_INFINITY;
    for (int i = 1, n = AsUnion()->Length(); i < n; ++i) {
      min = std::min(min, AsUnion()->Get(i).Min());
    }
    Type bitset = AsUnion()->Get(0);
    if (!bitset.Is(NaN())) min = std::min(min, bitset.Min());
    return min;
  }
  if (this->IsRange()) return this->AsRange()->Min();
  DCHECK(this->IsOtherNumberConstant());
  return this->AsOtherNumberConstant()->Value();
}

//double Max() const;*/

// -----------------------------------------------------------------------------
// Predicates.

// TODO:
bool SimplyEquals(v8type simplyequals_this_, v8type simplyequals_that) {
  /*DisallowGarbageCollection no_gc;
  if (this->IsHeapConstant()) {
    return that.IsHeapConstant() &&
           this->AsHeapConstant()->Value().address() ==
               that.AsHeapConstant()->Value().address();
  }
  if (this->IsOtherNumberConstant()) {
    return that.IsOtherNumberConstant() &&
           this->AsOtherNumberConstant()->Value() ==
               that.AsOtherNumberConstant()->Value();
  }
  if (this->IsRange()) {
    if (that.IsHeapConstant() || that.IsOtherNumberConstant()) return false;
  }
  if (this->IsTuple()) {
    if (!that.IsTuple()) return false;
    const TupleType* this_tuple = this->AsTuple();
    const TupleType* that_tuple = that.AsTuple();
    if (this_tuple->Arity() != that_tuple->Arity()) {
      return false;
    }
    for (int i = 0, n = this_tuple->Arity(); i < n; ++i) {
      if (!this_tuple->Element(i).Equals(that_tuple->Element(i))) return false;
    }
    return true;
  }*/
  // I think for us this is always false for now
  return (bool)0;
  //UNREACHABLE();
}

bool Maybe(v8Type maybe_this_, v8Type maybe_that) {
  //DisallowGarbageCollection no_gc;

  if (BitsetIsNone(BitsetLub(maybe_this_) & BitsetLub(maybe_that))) return false;

  // TODO: no unions what do?
  // (T1 \/ ... \/ Tn) overlaps T  if  (T1 overlaps T) \/ ... \/ (Tn overlaps T)
  /*if (this->IsUnion()) {
    for (int i = 0, n = this->AsUnion()->Length(); i < n; ++i) {
      if (this->AsUnion()->Get(i).Maybe(that)) return true;
    }
    return false;
  }*/

  // T overlaps (T1 \/ ... \/ Tn)  if  (T overlaps T1) \/ ... \/ (T overlaps Tn)
  /*if (that.IsUnion()) {
    for (int i = 0, n = that.AsUnion()->Length(); i < n; ++i) {
      if (this->Maybe(that.AsUnion()->Get(i))) return true;
    }
    return false;
  }*/

  // for now, just treat unions as "unbounded union of all types"
  if (IsUnion(maybe_this_)) {
      return true;
  }

  if (IsUnion(maybe_that_)) {
    return true;
  }

  if (IsBitset(maybe_this_) && IsBitset(maybe_that)) return true;

  if (IsRange(maybe_this_)) {
    if (IsRange(maybe_that)) {
      return Overlap(maybe_this_, maybe_that);
    }
    if (IsBitset(maybe_that)) {
      bitset_t maybe_number_bits = NumberBits(maybe_that.bitset);
      if (maybe_number_bits == kNone) {
        return false;
      }
      double maybe_min = math::max(BitsetMin(maybe_number_bits), Min(maybe_this_));
      double maybe_max = math::min(BitsetMax(maybe_number_bits), Max(maybe_this_));
      return maybe_min <= maybe_max;
    }
  }
  if (IsRange(maybe_that)) {
    return Maybe(maybe_that, maybe_this_);  // This case is handled above.
  }

  if (IsBitset(maybe_this_) || IsBitset(maybe_that)) return true;

  return SimplyEquals(maybe_this_, maybe_that);
}

//https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=520
bool SlowIs(v8type slowis_this_, v8type slowis_that_){
  if(IsBitset(slowis_that_)){
    return BitsetIs(BitsetLub(slowis_this_), AsBitset(slowis_that_));
  }

  if(IsBitset(slowis_this_)){
    return BitsetIs(AsBitset(slowis_this_), BitsetGlb(slowis_that_));
  }

  if(IsUnion(slowis_this_)){
    //TODO, for now return true if that_ is a union
    return IsUnion(slowis_that_);
  } 
  if(IsUnion(slowis_that_)){
    //TODO, for now return true if this_ is union
    //this looks redundant but once we handle unions this will actually do something different
    return IsUnion(slowis_this_);
  }
  if(IsRange(slowis_that_)){
    return IsRange(slowis_this_) && RangeContains(slowis_this_, slowis_that_);
  }
  if(IsRange(slowis_this_)){
    return FALSE;
  }
  return SimplyEquals(slowis_this_, slowis_that_);
}

//https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=48
bool RangeContains(v8type rangecontains_lhs, v8type rangecontains_rhs){
  return rangecontains_lhs.min <= rangecontains_rhs.min && rangecontains_rhs.max <= rangecontains_lhs.max;
}

//https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.h;l=394
bool Is(v8type is_this_, v8type is_that_){
  //NOTE this is simplified, the first condition here can actually be pointers to complicated
  //types but we don't have that yet
  //TODO: determine whether this matters
  return is_this_.bitset == is_that_.bitset || SlowIs(is_this_, is_that_);
}

// DONT EXPRESS NAN PRECONDITION YEET
// Rangers
v8type AddRanger(double addranger_lhs_min, double addranger_lhs_max, double addranger_rhs_min, double addranger_rhs_max) {
  double addranger_results_0 = addranger_lhs_min + addranger_rhs_min;
  double addranger_results_1 = addranger_lhs_min + addranger_rhs_max;
  double addranger_results_2 = addranger_lhs_max + addranger_rhs_min;
  double addranger_results_3 = addranger_lhs_max + addranger_rhs_max;
  // Since none of the inputs can be -0, the result cannot be -0 either.
  // However, it can be nan (the sum of two infinities of opposite sign).
  // On the other hand, if none of the "results" above is nan, then the
  // actual result cannot be nan either.
  uint32_t addranger_nans = (uint32_t)0;
  if (std::isnan(addranger_results_0)) {
      addranger_nans += (uint32_t)1;
  }
  if (std::isnan(addranger_results_1)) {
      addranger_nans += (uint32_t)1;
  }
  if (std::isnan(addranger_results_2)) {
      addranger_nans += (uint32_t)1;
  }
  if (std::isnan(addranger_results_3)) {
      addranger_nans += (uint32_t)1;
  }
  if (addranger_nans == (uint32_t)4) {
      return nanType();
  }
  //Type type = Type::Range(array_min(results, 4), array_max(results, 4), zone());
  v8type addranger_type = newRange(min4(addranger_results_0, addranger_results_1, addranger_results_2, addranger_results_3), max4(addranger_results_0, addranger_results_1, addranger_results_2, addranger_results_3));
  if (addranger_nans > (uint32_t)0) {
      addranger_type.maybeNaN = (bool)1;
  }
  //// Examples:
  ////   [-inf, -inf] + [+inf, +inf] = NaN
  ////   [-inf, -inf] + [n, +inf] = [-inf, -inf] \/ NaN
  ////   [-inf, +inf] + [n, +inf] = [-inf, +inf] \/ NaN
  ////   [-inf, m] + [n, +inf] = [-inf, +inf] \/ NaN
  return addranger_type;
}

v8type SubtractRanger(double subtractranger_lhs_min, double subtractranger_lhs_max, double subtractranger_rhs_min, double subtractranger_rhs_max) {
  double subtractranger_results_0 = subtractranger_lhs_min - subtractranger_rhs_min;
  double subtractranger_results_1 = subtractranger_lhs_min - subtractranger_rhs_max;
  double subtractranger_results_2 = subtractranger_lhs_max - subtractranger_rhs_min;
  double subtractranger_results_3 = subtractranger_lhs_max - subtractranger_rhs_max;
  // Since none of the inputs can be -0, the result cannot be -0 either.
  // However, it can be nan (the sum of two infinities of opposite sign).
  // On the other hand, if none of the "results" above is nan, then the
  // actual result cannot be nan either.
  uint32_t subtractranger_nans = (uint32_t)0;
  if (std::isnan(subtractranger_results_0)) {
      subtractranger_nans += (uint32_t)1;
  }
  if (std::isnan(subtractranger_results_1)) {
      subtractranger_nans += (uint32_t)1;
  }
  if (std::isnan(subtractranger_results_2)) {
      subtractranger_nans += (uint32_t)1;
  }
  if (std::isnan(subtractranger_results_3)) {
      subtractranger_nans += (uint32_t)1;
  }
  if (subtractranger_nans == (uint32_t)4) {
      return nanType();
  }
  //Type type = Type::Range(array_min(results, 4), array_max(results, 4), zone());
  v8type subtractranger_type = newRange(min4(subtractranger_results_0, subtractranger_results_1, subtractranger_results_2, subtractranger_results_3), max4(subtractranger_results_0, subtractranger_results_1, subtractranger_results_2, subtractranger_results_3));
  if (subtractranger_nans > (uint32_t)0) {
      subtractranger_type.maybeNaN = (bool)1;
  }
  // Examples:
  //   [-inf, +inf] - [-inf, +inf] = [-inf, +inf] \/ NaN
  //   [-inf, -inf] - [-inf, -inf] = NaN
  //   [-inf, -inf] - [n, +inf] = [-inf, -inf] \/ NaN
  //   [m, +inf] - [-inf, n] = [-inf, +inf] \/ NaN
  return subtractranger_type;
}

v8type MultiplyRanger(double multiplyranger_lhs_min, double multiplyranger_lhs_max, double multiplyranger_rhs_min, double multiplyranger_rhs_max) {
  double multiplyranger_results_0 = multiplyranger_lhs_min * multiplyranger_rhs_min;
  double multiplyranger_results_1 = multiplyranger_lhs_min * multiplyranger_rhs_max;
  double multiplyranger_results_2 = multiplyranger_lhs_max * multiplyranger_rhs_min;
  double multiplyranger_results_3 = multiplyranger_lhs_max * multiplyranger_rhs_max;
  // If the result may be nan, we give up on calculating a precise type,
  // because the discontinuity makes it too complicated.  Note that even if
  // none of the "results" above is nan, the actual result may still be, so we
  // have to do a different check:
  // TODO: change this probably
  if (std::isnan(multiplyranger_results_0)) {
      return nanType();
  }
  if (std::isnan(multiplyranger_results_1)) {
      return nanType();
  }
  if (std::isnan(multiplyranger_results_2)) {
      return nanType();
  }
  if (std::isnan(multiplyranger_results_3)) {
      return nanType();
  }

  double multiplyranger_min = min4(multiplyranger_results_0, multiplyranger_results_1, multiplyranger_results_2, multiplyranger_results_3);
  double multiplyranger_max = max4(multiplyranger_results_0, multiplyranger_results_1, multiplyranger_results_2, multiplyranger_results_3);
  v8type multiplyranger_type = newRange(multiplyranger_min, multiplyranger_max);
  // HACK: no && allowed .... :(
  if (multiplyranger_min <= (double)0.0) {
      if ((double)0.0 <= multiplyranger_max) {
          if (multiplyranger_lhs_min < (double)0.0) {
            multiplyranger_type.maybeMinusZero = (bool)1;
          } 
          if (multiplyranger_rhs_min < (double)0.0) {
            multiplyranger_type.maybeMinusZero = (bool)1;
          }
      }
  }
  // 0 * V8_INFINITY is NaN, regardless of sign
  if (((multiplyranger_lhs_min == -V8_INFINITY || multiplyranger_lhs_max == V8_INFINITY) &&
       (multiplyranger_rhs_min <= 0.0 && 0.0 <= multiplyranger_rhs_max)) ||
      ((multiplyranger_rhs_min == -V8_INFINITY || multiplyranger_rhs_max == V8_INFINITY) &&
       (multiplyranger_lhs_min <= 0.0 && 0.0 <= multiplyranger_lhs_max))) {
    multiplyranger_type.maybeNaN = TRUE;
  }
  return multiplyranger_type;
}

limits IntersectAux(v8type intersectaux_lhs, v8type intersectaux_rhs, limits intersectaux_incomingLimit){
  //first two if statements can be ignored since we don't support unions
  if(BitsetIsNone(BitsetLub(intersectaux_lhs) & BitsetLub(intersectaux_rhs))){
    return intersectaux_incomingLimit; 
  }

  bool intersectaux_continue = FALSE;
  // delcare vars in if statement...
  limits intersectaux_lhs_lim;
  limits intersectaux_rhs_lim;
  limits intersectaux_newLimit;
  intersectaux_lhs_lim.min = 0.0;
  intersectaux_lhs_lim.max = 0.0;
  intersectaux_rhs_lim.min = 0.0;
  intersectaux_rhs_lim.max = 0.0;
  intersectaux_newLimit.min = 0.0;
  intersectaux_newLimit.max = 0.0;
  limits intersectaux_union; 
  intersectaux_union.min = 0.0;
  intersectaux_union.max = 0.0;
  if(IsRange(intersectaux_lhs)){
    if(IsBitset(intersectaux_rhs)){

    } 
    if(IsRange(intersectaux_rhs)){
      intersectaux_lhs_lim = getLimits(intersectaux_lhs);
      intersectaux_rhs_lim = getLimits(intersectaux_rhs);
      intersectaux_newLimit = LimitIntersect(intersectaux_lhs_lim, intersectaux_rhs_lim);
      if(!IsEmpty(intersectaux_newLimit)){
        intersectaux_union = Union(intersectaux_newLimit, intersectaux_incomingLimit);
        intersectaux_continue = FALSE;
        return intersectaux_union; 
      } else {
        intersectaux_continue = FALSE;
        return intersectaux_incomingLimit;
      }
    }
  }

  limits intersectaux_lhs_lim1;
  limits intersectaux_rhs_lim1;
  limits intersectaux_newLimit1;
  intersectaux_lhs_lim1.min = 0.0;
  intersectaux_lhs_lim1.max = 0.0;
  intersectaux_rhs_lim1.min = 0.0;
  intersectaux_rhs_lim1.max = 0.0;
  intersectaux_newLimit1.min = 0.0;
  intersectaux_newLimit1.max = 0.0;
  limits intersectaux_union1; 
  intersectaux_union1.min = 0.0;
  intersectaux_union1.max = 0.0;
  if(IsRange(intersectaux_rhs)){
    if(IsBitset(intersectaux_lhs)){

    } 
    if(IsRange(intersectaux_lhs)){
      intersectaux_rhs_lim1 = getLimits(intersectaux_rhs);
      intersectaux_lhs_lim1 = getLimits(intersectaux_lhs);
      intersectaux_newLimit1 = LimitIntersect(intersectaux_rhs_lim1, intersectaux_lhs_lim1);
      if(!IsEmpty(intersectaux_newLimit1) && intersectaux_continue){
        intersectaux_union1 = Union(intersectaux_newLimit1, intersectaux_incomingLimit);
        return intersectaux_union1; 
      } else if (intersectaux_continue) {
        return intersectaux_incomingLimit;
      }
    }
  }

  //last two cases we don't handle because they involve unions again
  //TODO is this correct?
  return intersectaux_incomingLimit;
}


v8type NormalizeUnion(v8type normalizeunion_union){ //this is a no-op currently since we don't support unions
  return normalizeunion_union;
}

// ignore zone
//Type Type::Intersect(Type type1, Type type2, Zone* zone) {
v8type Intersect(v8type const& intersect_type1, v8type const& intersect_type2) {
  //return noneType();
  bool intersect_continue = TRUE;
  v8type intersect_result = AnyType(); //NewBitset(AsBitset(intersect_type1) & AsBitset(intersect_type2));
  // Fast case: bit sets.
  if (IsBitset(intersect_type1) && IsBitset(intersect_type2)) {
      intersect_continue = FALSE;
      intersect_result = NewBitset(AsBitset(intersect_type1) & AsBitset(intersect_type2));
  }

  // Fast case: top or bottom types.
  if (intersect_continue && (TypeIsNone(intersect_type1) || TypeIsAny(intersect_type2))) {
      intersect_continue = FALSE;
      intersect_result = intersect_type1;  // Shortcut.
  }
  if (intersect_continue && (TypeIsNone(intersect_type2) || TypeIsAny(intersect_type1))) {
      intersect_continue = FALSE;
      intersect_result = intersect_type2;  // Shortcut.
  }

  // Semi-fast case.
  if (intersect_continue && (Is(intersect_type1, intersect_type2))) {
      intersect_continue = FALSE;
      intersect_result = intersect_type1;
  }
  if (intersect_continue && (Is(intersect_type2, intersect_type1))) {
      intersect_continue = FALSE;
      intersect_result = intersect_type2;
  }

  // Slow case: create union.

  // Semantic subtyping check - this is needed for consistency with the
  // semi-fast case above.
  if (Is(intersect_type1, intersect_type2)) {
    intersect_type2 = AnyType();
  } else if (Is(intersect_type2, intersect_type1)) {
    intersect_type1 = AnyType();
  }

 
  bitset_t intersect_bits = BitsetGlb(intersect_type1) & BitsetGlb(intersect_type2);
  int32_t intersect_size1 = (int32_t)1; //we don't support unions so this is just 1
  int32_t intersect_size2 = (int32_t)1; //also changed from int to int32_t

  if(intersect_continue && (SignedAddWouldOverflow32(intersect_size1, intersect_size2))){
    intersect_continue = FALSE;
    intersect_result = AnyType();
  }

  int32_t intersect_size = intersect_size1 + intersect_size2;
 
  if(intersect_continue && (SignedAddWouldOverflow32(intersect_size, (int32_t)2))){
    intersect_continue = FALSE;
    intersect_result = AnyType();
  }

  intersect_size = intersect_size + (int32_t)1;
  limits intersect_rhs_lim = getLimits(intersect_type1);
  limits intersect_lhs_lim = getLimits(intersect_type2);

  limits intersect_lims = LimitIntersect(intersect_rhs_lim, intersect_lhs_lim);
  if (intersect_continue && (IsRange(intersect_type1) && IsRange(intersect_type2))) {
    /*limits intersect_lims = IntersectAux(intersect_type1, intersect_type2, intersect_empty);*/
    //update result, normally done in IntersectAux but lack of pointers means we do it this way
    v8type intersect_result = newRange(intersect_lims.min, intersect_lims.max);
  }

  /*bitset_t intersect_number_bits = UINT32_ZERO;
  if(!IsEmpty(intersect_lims)){
    //don't need UpdateRange because we don't support unions
    intersect_number_bits = NumberBits(intersect_bits);
    intersect_bits = intersect_bits & ~intersect_number_bits;
    intersect_result.bitset = intersect_bits;
  }*/
  
  return intersect_result;
  //return AnyType();
  /*
  if (base::bits::SignedAddOverflow32(size1, size2, &size)) return Any();
  if (base::bits::SignedAddOverflow32(size, 2, &size)) return Any();
  UnionType* result = UnionType::New(size, zone);
  size = 0;

  // Deal with bitsets.
  result->Set(size++, NewBitset(bits));

  RangeType::Limits lims = RangeType::Limits::Empty();
  size = IntersectAux(type1, type2, result, size, &lims, zone);

  // If the range is not empty, then insert it into the union and
  // remove the number bits from the bitset.
  if (!lims.IsEmpty()) {
    size = UpdateRange(Type::Range(lims, zone), result, size, zone);

    // Remove the number bits.
    bitset number_bits = BitsetType::NumberBits(bits);
    bits &= ~number_bits;
    result->Set(0, NewBitset(bits));
  }
  return NormalizeUnion(result, size, zone);*/
}

v8Type TypeUnion(v8Type typeunion_type1, v8Type typeunion_type2) {
  //TODO, need to handle ranges at least a little bit probably
  // Fast case: bit sets.
  if (IsBitset(typeunion_type1) && IsBitset(typeunion_type2)) {
    return newBitset(typeunion_type1.bitset | typeunion_type2.bitset);
  }

  // Fast case: top or bottom types.
  if (TypeIsAny(typeunion_type1) || TypeIsNone(typeunion_type2)) return typeunion_type1;
  if (TypeIsAny(typeunion_type2)  || TypeIsNone(typeunion_type1)) return typeunion_type2;

  // Semi-fast case.
  if (Is(typeunion_type1, typeunion_type2)) return typeunion_type1;
  if (Is(typeunion_type2, typeunion_type1)) return type2;

  return AnyType();

  //   // Slow case: create union.
  //   int size1 = type1.IsUnion() ? type1.AsUnion()->Length() : 1;
  //   int size2 = type2.IsUnion() ? type2.AsUnion()->Length() : 1;
  //   int size;
  //   if (base::bits::SignedAddOverflow32(size1, size2, &size)) return Any();
  //   if (base::bits::SignedAddOverflow32(size, 2, &size)) return Any();
  //   UnionType* result = UnionType::New(size, zone);
  //   size = 0;

  //   // Compute the new bitset.
  //   bitset new_bitset = type1.BitsetGlb() | type2.BitsetGlb();

  //   // Deal with ranges.
  //   Type range = None();
  //   Type range1 = type1.GetRange();
  //   Type range2 = type2.GetRange();
  //   if (range1 != nullptr && range2 != nullptr) {
  //     RangeType::Limits lims =
  //         RangeType::Limits::Union(RangeType::Limits(range1.AsRange()),
  //                                  RangeType::Limits(range2.AsRange()));
  //     Type union_range = Type::Range(lims, zone);
  //     range = NormalizeRangeAndBitset(union_range, &new_bitset, zone);
  //   } else if (range1 != nullptr) {
  //     range = NormalizeRangeAndBitset(range1, &new_bitset, zone);
  //   } else if (range2 != nullptr) {
  //     range = NormalizeRangeAndBitset(range2, &new_bitset, zone);
  //   }
  //   Type bits = NewBitset(new_bitset);
  //   result->Set(size++, bits);
  //   if (!range.IsNone()) result->Set(size++, range);

  //   size = AddToUnion(type1, result, size, zone);
  //   size = AddToUnion(type2, result, size, zone);
  //   return NormalizeUnion(result, size, zone);
}


// Precondition: input is numbers
v8type NumberAdd(v8type numberadd_lhs, v8type numberadd_rhs) {
  //DCHECK(lhs.Is(Type::Number()));
  //DCHECK(numberadd_rhs.Is(Type::Number()));

  if (TypeIsNone(numberadd_lhs) || TypeIsNone(numberadd_rhs)) {
    return noneType();
  }

  // Addition can return NaN if either input can be NaN or we try to compute
  // the sum of two infinities of opposite sign.
  v8type numberadd_nantype = nanType();
  v8type numberadd_minuszerotype = minusZeroType();
  v8type numberadd_singletonZero = kSingletonZero;
  v8type numberadd_integer = kInteger;
  bool maybe_nan = Maybe(numberadd_lhs, numberadd_nantype) || Maybe(numberadd_rhs, numberadd_nantype);

  // Addition can yield minus zero only if both inputs can be minus zero.
  bool maybe_minuszero = true;
  if (Maybe(numberadd_lhs, numberadd_minuszerotype)) {
    numberadd_lhs = TypeUnion(numberadd_lhs, numberadd_singletonZero); //we don't do unions yet so just merge ranges
  } else {
    maybe_minuszero = false;
  }
  if (Maybe(numberadd_rhs, numberadd_minuszerotype)) {
    numberadd_rhs = TypeUnion(numberadd_rhs, numberadd_singletonZero);
  } else {
    maybe_minuszero = false;
  }

  // We can give more precise types for integers.
  v8type type = noneType();
  numberadd_lhs = Intersect(numberadd_lhs, plainNumberType());
  numberadd_rhs = Intersect(numberadd_rhs, plainNumberType());
  if (!TypeIsNone(numberadd_lhs) && !TypeIsNone(numberadd_rhs)) {
    if (Is(numberadd_lhs, numberadd_integer) && Is(numberadd_rhs, numberadd_integer)) {
      type = AddRanger(numberadd_lhs.min, numberadd_lhs.max, numberadd_rhs.min, numberadd_rhs.max);
    } else {
      if ((Maybe(numberadd_lhs, minusInfinityType()) && Maybe(numberadd_rhs, infinityType())) || // minus_infinity_, infinity_ are Types
          (Maybe(numberadd_rhs,minusInfinityType()) && Maybe(numberadd_lhs, infinityType()))) {
        maybe_nan = true;
      }
      type = plainNumberType();
    }
  }

  // Take into account the -0 and NaN information computed earlier.
  if (maybe_minuszero) {
    type = TypeUnion(type, numberadd_minuszerotype);
  }
  if (maybe_nan) {
    type = TypeUnion(type,numberadd_nantype);
  }
  return type;
}

