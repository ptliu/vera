
#define V8_INFINITY ((double)9218868437227405312)
#define kMaxInt ((int32_t) 2147483647)
#define kMinInt ((int32_t) -kMaxInt - (int32_t)1)
#define kMaxUInt32 ((uint32_t) 4294967295)

#define bitset_t uint32_t

// define internal bitset types
#define OtherUnsigned31     (uint32_t)1 << (uint32_t)1
#define OtherUnsigned32     (uint32_t)1 << (uint32_t)2
#define OtherSigned32       (uint32_t)1 << (uint32_t)3
#define OtherNumber         (uint32_t)1 << (uint32_t)4
#define OtherString         (uint32_t)1 << (uint32_t)5

// define external bitset types
#define None                     (uint32_t)0
#define Negative31               (uint32_t)1 << (uint32_t)6
#define Null                     (uint32_t)1 << (uint32_t)7
#define Undefined                (uint32_t)1 << (uint32_t)8
#define Boolean                  (uint32_t)1 << (uint32_t)9
#define Unsigned30               (uint32_t)1 << (uint32_t)10
#define MinusZero                (uint32_t)1 << (uint32_t)11
#define NaN                      (uint32_t)1 << (uint32_t)12
#define Symbol                   (uint32_t)1 << (uint32_t)13
#define InternalizedString       (uint32_t)1 << (uint32_t)14
#define OtherCallable            (uint32_t)1 << (uint32_t)16
#define OtherObject              (uint32_t)1 << (uint32_t)17
#define OtherUndetectable        (uint32_t)1 << (uint32_t)18
#define CallableProxy            (uint32_t)1 << (uint32_t)19
#define OtherProxy               (uint32_t)1 << (uint32_t)20
#define Function                 (uint32_t)1 << (uint32_t)21
#define BoundFunction            (uint32_t)1 << (uint32_t)22
#define Hole                     (uint32_t)1 << (uint32_t)23
#define OtherInternal            (uint32_t)1 << (uint32_t)24
#define ExternalPointer          (uint32_t)1 << (uint32_t)25
#define Array                    (uint32_t)1 << (uint32_t)26
#define BigInt                   (uint32_t)1 << (uint32_t)27
  /* TODO(v8:10391): Remove this type once all ExternalPointer usages are */ \
  /* sandbox-ready. */                   \
#define SandboxedExternalPointer (uint32_t)1 << (uint32_t)28
#define Signed31                     kUnsigned30 | kNegative31
#define Signed32                     kSigned31 | kOtherUnsigned31 | \
                                  kOtherSigned32
#define Signed32OrMinusZero          kSigned32 | kMinusZero
#define Signed32OrMinusZeroOrNaN     kSigned32 | kMinusZero | kNaN
#define Negative32                   kNegative31 | kOtherSigned32
#define Unsigned31                   kUnsigned30 | kOtherUnsigned31
#define Unsigned32                   kUnsigned30 | kOtherUnsigned31 | \
                                  kOtherUnsigned32
#define Unsigned32OrMinusZero        kUnsigned32 | kMinusZero
#define Unsigned32OrMinusZeroOrNaN   kUnsigned32 | kMinusZero | kNaN
#define Integral32                   kSigned32 | kUnsigned32
#define Integral32OrMinusZero        kIntegral32 | kMinusZero
#define Integral32OrMinusZeroOrNaN   kIntegral32OrMinusZero | kNaN
#define PlainNumber                  kIntegral32 | kOtherNumber
#define OrderedNumber                kPlainNumber | kMinusZero
#define MinusZeroOrNaN               kMinusZero | kNaN
#define Number                       kOrderedNumber | kNaN
#define Numeric                      kNumber | kBigInt
#define String                       kInternalizedString | kOtherString
#define UniqueName                   kSymbol | kInternalizedString
#define Name                         kSymbol | kString
#define InternalizedStringOrNull     kInternalizedString | kNull
#define BooleanOrNumber              kBoolean | kNumber
#define BooleanOrNullOrNumber        kBooleanOrNumber | kNull
#define BooleanOrNullOrUndefined     kBoolean | kNull | kUndefined
#define Oddball                      kBooleanOrNullOrUndefined | kHole
#define NullOrNumber                 kNull | kNumber
#define NullOrUndefined              kNull | kUndefined
#define Undetectable                 kNullOrUndefined | kOtherUndetectable
#define NumberOrHole                 kNumber | kHole
#define NumberOrOddball              kNumber | kNullOrUndefined | kBoolean | \
                                     kHole
#define NumericOrString              kNumeric | kString
#define NumberOrUndefined            kNumber | kUndefined
#define NumberOrUndefinedOrNullOrBoolean  \
                                     kNumber | kNullOrUndefined | kBoolean
#define PlainPrimitive               kNumber | kString | kBoolean | \
                                     kNullOrUndefined
#define NonBigIntPrimitive           kSymbol | kPlainPrimitive
#define Primitive                    kBigInt | kNonBigIntPrimitive
#define OtherUndetectableOrUndefined kOtherUndetectable | kUndefined
#define Proxy                        kCallableProxy | kOtherProxy
#define ArrayOrOtherObject           kArray | kOtherObject
#define ArrayOrProxy                 kArray | kProxy
#define DetectableCallable           kFunction | kBoundFunction | \
                                     kOtherCallable | kCallableProxy
#define Callable                     kDetectableCallable | kOtherUndetectable
#define NonCallable                  kArray | kOtherObject | kOtherProxy
#define NonCallableOrNull            kNonCallable | kNull
#define DetectableObject             kArray | kFunction | kBoundFunction | \
                                     kOtherCallable | kOtherObject
#define DetectableReceiver           kDetectableObject | kProxy
#define DetectableReceiverOrNull     kDetectableReceiver | kNull
#define Object                       kDetectableObject | kOtherUndetectable
#define Receiver                     kObject | kProxy
#define ReceiverOrUndefined          kReceiver | kUndefined
#define ReceiverOrNullOrUndefined    kReceiver | kNull | kUndefined
#define SymbolOrReceiver             kSymbol | kReceiver
#define StringOrReceiver             kString | kReceiver
#define Unique                       kBoolean | kUniqueName | kNull | \
                                     kUndefined | kHole | kReceiver
#define Internal                     kHole | kExternalPointer | \
                                     kSandboxedExternalPointer | kOtherInternal
#define NonInternal                  kPrimitive | kReceiver
#define NonBigInt                    kNonBigIntPrimitive | kReceiver
#define NonNumber                    kBigInt | kUnique | kString | kInternal
#define Any                          (uint32_t)0xfffffffe

struct limit {
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

// Helpers
double min4(double one, double two, double three, double four) {
    double min = one;
    if (two < min) {
        min = two;
    }
    if (three < min) {
        min = three;
    } 
    if (four < min) {
        min = four;
    }

    return min;
}

double max4(double one, double two, double three, double four) {
    double max = one;
    if (two > max) {
        max = two;
    }
    if (three > max) {
        max = three;
    } 
    if (four > max) {
        max = four;
    }

    return max;
}

v8type newRange(double min, double max) {
    v8type type;
    type.bitset = (uint32_t)0;
    type.hasRange = (bool)1;
    type.max = max;
    type.min = min;
    type.maybeNaN = (bool)0;
    type.maybeMinusZero = (bool)0;
    type.isUnion = (bool)0;
    return type;
}

v8type nanType() {
    v8type type;
    type.bitset = (uint32_t)0;
    type.hasRange = (bool)0;
    type.max = max;
    type.min = min;
    type.maybeNaN = (bool)1;
    type.maybeMinusZero = (bool)0;
    type.isUnion = (bool)0;
    return type;
}

limits copy(limits other) {
    limits result;
    limits.min = other.min;
    limits.max = other.max;
    return result;
}

limits getLimits(v8type const& t) {
    limits result;
    limits.min = t.min; // TODO: wrong!
    limits.max = t.max;
    return result;
}

// boundary helpers

boundary getBoundary(uint32_t index) {
    boundary bound;
    if (index == (uint32_t)0) {
        bound.internal = kOtherNumber;
        bound.external = kPlainNumber;
        bound.min = (double)-V8_INFINITY;
    }
    if (index == (uint32_t)1) {
        bound.internal = kOtherSigned32;
        bound.external = kNegative32;
        bound.min = (double)kMinInt;
    }
    if (index == (uint32_t)2) {
        bound.internal = kNegative31;
        bound.external = kNegative31;
        bound.min = (double)-1073741824;
    }
    if (index == (uint32_t)3) {
        bound.internal = kUnsigned30;
        bound.external = kUnsigned30;
        bound.min = (double)0;
    }
    if (index == (uint32_t)4) {
        bound.internal = kOtherUnsigned31;
        bound.external = kUnsigned31;
        bound.min = (double)1073741824;
    }
    if (index == (uint32_t)5) {
        bound.internal = kOtherUnsigned32;
        bound.external = kUnsigned32;
        bound.min = (double)2147483648;
    }
    if (index == (uint32_t)6) {
        bound.internal = kOtherNumber;
        bound.external = kPlainNumber;
        bound.min = (double)kMaxUInt32 + (double)1;
    }

    return bound;
}

uint32_t BoundariesSize() {
    return (uint32_t)7;
}

// Range-related helper functions
//

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=22
bool IsEmpty(limits this_) { 
    return this_.min > this_.max;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=24
limits Intersect(limits lhs, limits rhs) {
  //DisallowGarbageCollection no_gc;
  limits result = copy(lhs);

  if (lhs.min < rhs.min) {
      result.min = rhs.min;
  }
  if (lhs.max > rhs.max) { 
      result.max = rhs.max;
  }
  return result;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=32
limits Union(limits lhs, limits rhs) {
  //DisallowGarbageCollection no_gc;
  if (IsEmpty(lhs)) {
      return rhs;
  }
  if (IsEmpty(rhs)) {
      return lhs;
  }
  limits result = copy(lhs);
  if (lhs.min > rhs.min) {
    result.min = rhs.min;
  }
  if (lhs.max < rhs.max) {
      result.max = rhs.max;
  }
  return result;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=42
bool Overlap(v8type const& lhs, v8type const& rhs) {
  //DisallowGarbageCollection no_gc;
  return !IsEmpty(Intersect(getLimits(lhs),
                    getLimits(rhs)));
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=49
// TODO
/*bool Type::Contains(const v8type& lhs, const v8type& rhs) {
  //DisallowGarbageCollection no_gc;
  return lhs->Min() <= rhs->Min() && rhs->Max() <= lhs->Max();
}*/

// Oversimplified IsBitset. Can't do original cause we don't have
// type hierarchy
bool IsBitset(v8type const& this_) {
    // TODO:
    return this_.hasRange || this_.isUnion;
}

bool IsRange(v8type const& this_) {
    return this_.hasRange && !this_.isUnion;
}

bool IsUnion(v8type const& this_) {
    return this_.isUnion;
}

// Bitset methods

bool BitsetIsNone(bitset_t bits) {
    return bits == kNone;
}

bool BitsetIs(bitset_t bits1, bitset_t bits2) {
    return (bits1 | bits2) == bits2;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=438
double BitsetMin(bitset_t bits) {
  //DisallowGarbageCollection no_gc;
  //DCHECK(Is(bits, kNumber));
  //DCHECK(!Is(bits, kNaN));
  //const Boundary* mins = Boundaries();
  
  bool mz = bits & kMinusZero;

  // DELEGATED TO minBoundary helper, cause we dont got loops...
  // unroll the loop
  boundary min = getBoundary((uint32_t)0);
  if (BitsetIs(min.internal, bits)) {
    return mz ? math::min(0.0, min.min) : min.min;
  }
  boundary min = getBoundary((uint32_t)1);
  if (BitsetIs(min.internal, bits)) {
    return mz ? math::min(0.0, min.min) : min.min;
  }
  boundary min = getBoundary((uint32_t)2);
  if (BitsetIs(min.internal, bits)) {
    return mz ? math::min(0.0, min.min) : min.min;
  }
  boundary min = getBoundary((uint32_t)3);
  if (BitsetIs(min.internal, bits)) {
    return mz ? math::min(0.0, min.min) : min.min;
  }
  boundary min = getBoundary((uint32_t)4);
  if (BitsetIs(min.internal, bits)) {
    return mz ? math::min(0.0, min.min) : min.min;
  }
  boundary min = getBoundary((uint32_t)5);
  if (BitsetIs(min.internal, bits)) {
    return mz ? math::min(0.0, min.min) : min.min;
  }
  boundary min = getBoundary((uint32_t)6);
  if (BitsetIs(min.internal, bits)) {
    return mz ? math::min(0.0, min.min) : min.min;
  }

  //DCHECK(mz);
  return (double)0;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=453;bpv=0;bpt=1
double BitsetMax(bitset_t bits) {
  //DisallowGarbageCollection no_gc;
  //DCHECK(Is(bits, kNumber));
  //DCHECK(!Is(bits, kNaN));
  
  bool mz = bits & kMinusZero;

  if (BitsetIs(getBoundary(BoundariesSize() - 1).internal, bits)) {
    return +V8_INFINITY;
  }

  // unrolled
  /*for (size_t i = BoundariesSize() - 1; i-- > 0;) {
    if (Is(mins[i].internal, bits)) {
      return mz ? std::max(0.0, mins[i + 1].min - 1) : mins[i + 1].min - 1;
    }
  }*/
  uint32_t i = (uint32_t)BoundariesSize() - 1;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+1);
    return mz ? math::min(0.0, min1.min - 1) : min1.min - 1;
  }
  i -= 1;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+1);
    return mz ? math::min(0.0, min1.min - 1) : min1.min - 1;
  }
  i -= 1;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+1);
    return mz ? math::min(0.0, min1.min - 1) : min1.min - 1;
  }
  i -= 1;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+1);
    return mz ? math::min(0.0, min1.min - 1) : min1.min - 1;
  }
  i -= 1;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+1);
    return mz ? math::min(0.0, min1.min - 1) : min1.min - 1;
  }
  i -= 1;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+1);
    return mz ? math::min(0.0, min1.min - 1) : min1.min - 1;
  }
  i -= 1;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+1);
    return mz ? math::min(0.0, min1.min - 1) : min1.min - 1;
  }

  DCHECK(mz);
  return 0;
}

bitset_t NumberBits(bitset_t bits) {
    return bits & kPlainNumber;
}

bitset_t BitsetLub(bitset_t this_) {
  // The smallest bitset subsuming this type, possibly not a proper one.

  // DisallowGarbageCollection no_gc;
  bool mz = this_ & kMinusZero;

  if (BitsetIs(this_, )) {
    return 
  }

  // if (IsBitset()) return AsBitset();

  // if (IsUnion()) {
  //   // Take the representation from the first element, which is always
  //   // a bitset.
  //   int bitset = AsUnion()->Get(0).BitsetLub();
  //   for (int i = 0, n = AsUnion()->Length(); i < n; ++i) {
  //     // Other elements only contribute their semantic part.
  //     bitset |= AsUnion()->Get(i).BitsetLub();
  //   }
  //   return bitset;
  // }

  // if (IsHeapConstant()) return AsHeapConstant()->Lub();
  // if (IsOtherNumberConstant()) {
  //   return AsOtherNumberConstant()->Lub();
  // }

  if (IsRange()) return AsRange()->Lub();
  if (IsTuple()) return BitsetType::kOtherInternal;
  UNREACHABLE();
}

}


// Type methods

// Minimum and maximum of a numeric type.
// These functions do not distinguish between -0 and +0.  NaN is ignored.
// Only call them on subtypes of Number whose intersection with OrderedNumber
// is not empty.
/*double Min(v8type const& this_) const {
  //DCHECK(this->Is(Number())); TODO
  //DCHECK(!this->Is(NaN()));
  if (IsBitset(this_)) {
      return BitsetMin(this_.bitset);
  }
  if (this->IsUnion()) {
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
bool SimplyEquals(v8type this_, v8type that) {
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

bool Maybe(v8Type this_, v8Type that) {
  //DisallowGarbageCollection no_gc;

  if (BitsetIsNone(this->BitsetLub() & that.BitsetLub())) return false;

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
  if (IsUnion(this_)) {
      return true;
  }

  if (IsUnion(that_)) {
    return true;
  }

  if (IsBitset(this_) && IsBitset(that)) return true;

  if (IsRange(this_)) {
    if (IsRange(that)) {
      return Overlap(this_, that);
    }
    if (IsBitset(that)) {
      bitset number_bits = BitsetType::NumberBits(that.bitset);
      if (number_bits == kNone) {
        return false;
      }
      double min = math::max(BitsetMin(number_bits), Min(this_));
      double max = math::min(BitsetMax(number_bits), Max(this_));
      return min <= max;
    }
  }
  if (IsRange(that)) {
    return Maybe(that, this_);  // This case is handled above.
  }

  if (IsBitset(this_) || IsBitset(that)) return true;

  return SimplyEquals(this_, that);
}


// DONT EXPRESS NAN PRECONDITION YEET
// Rangers
v8type AddRanger(double lhs_min, double lhs_max, double rhs_min, double rhs_max) {
  double results_0 = lhs_min + rhs_min;
  double results_1 = lhs_min + rhs_max;
  double results_2 = lhs_max + rhs_min;
  double results_3 = lhs_max + rhs_max;
  // Since none of the inputs can be -0, the result cannot be -0 either.
  // However, it can be nan (the sum of two infinities of opposite sign).
  // On the other hand, if none of the "results" above is nan, then the
  // actual result cannot be nan either.
  uint32_t nans = (uint32_t)0;
  if (std::isnan(results_0)) {
      nans += (uint32_t)1;
  }
  if (std::isnan(results_1)) {
      nans += (uint32_t)1;
  }
  if (std::isnan(results_2)) {
      nans += (uint32_t)1;
  }
  if (std::isnan(results_3)) {
      nans += (uint32_t)1;
  }
  if (nans == (uint32_t)4) {
      return nanType();
  }
  //Type type = Type::Range(array_min(results, 4), array_max(results, 4), zone());
  v8type type = newRange(min4(results_0, results_1, results_2, results_3), max4(results_0, results_1, results_2, results_3));
  if (nans > (uint32_t)0) {
      type.maybeNaN = (bool)1;
  }
  //// Examples:
  ////   [-inf, -inf] + [+inf, +inf] = NaN
  ////   [-inf, -inf] + [n, +inf] = [-inf, -inf] \/ NaN
  ////   [-inf, +inf] + [n, +inf] = [-inf, +inf] \/ NaN
  ////   [-inf, m] + [n, +inf] = [-inf, +inf] \/ NaN
  return type;
}

v8type SubtractRanger(double lhs_min, double lhs_max, double rhs_min, double rhs_max) {
  double results_0 = lhs_min - rhs_min;
  double results_1 = lhs_min - rhs_max;
  double results_2 = lhs_max - rhs_min;
  double results_3 = lhs_max - rhs_max;
  // Since none of the inputs can be -0, the result cannot be -0 either.
  // However, it can be nan (the sum of two infinities of opposite sign).
  // On the other hand, if none of the "results" above is nan, then the
  // actual result cannot be nan either.
  uint32_t nans = (uint32_t)0;
  if (std::isnan(results_0)) {
      nans += (uint32_t)1;
  }
  if (std::isnan(results_1)) {
      nans += (uint32_t)1;
  }
  if (std::isnan(results_2)) {
      nans += (uint32_t)1;
  }
  if (std::isnan(results_3)) {
      nans += (uint32_t)1;
  }
  if (nans == (uint32_t)4) {
      return nanType();
  }
  //Type type = Type::Range(array_min(results, 4), array_max(results, 4), zone());
  v8type type = newRange(min4(results_0, results_1, results_2, results_3), max4(results_0, results_1, results_2, results_3));
  if (nans > (uint32_t)0) {
      type.maybeNaN = (bool)1;
  }
  // Examples:
  //   [-inf, +inf] - [-inf, +inf] = [-inf, +inf] \/ NaN
  //   [-inf, -inf] - [-inf, -inf] = NaN
  //   [-inf, -inf] - [n, +inf] = [-inf, -inf] \/ NaN
  //   [m, +inf] - [-inf, n] = [-inf, +inf] \/ NaN
  return type;
}

v8type MultiplyRanger(double lhs_min, double lhs_max, double rhs_min, double rhs_max) {
  double results_0 = lhs_min * rhs_min;
  double results_1 = lhs_min * rhs_max;
  double results_2 = lhs_max * rhs_min;
  double results_3 = lhs_max * rhs_max;
  // If the result may be nan, we give up on calculating a precise type,
  // because the discontinuity makes it too complicated.  Note that even if
  // none of the "results" above is nan, the actual result may still be, so we
  // have to do a different check:
  // TODO: change this probably
  if (std::isnan(results_0)) {
      return nanType();
  }
  if (std::isnan(results_1)) {
      return nanType();
  }
  if (std::isnan(results_2)) {
      return nanType();
  }
  if (std::isnan(results_3)) {
      return nanType();
  }

  double min = min4(results_0, results_1, results_2, results_3);
  double max = max4(results_0, results_1, results_2, results_3);
  v8type type = newRange(min, max);
  // HACK: no && allowed .... :(
  if (min <= (double)0.0) {
      if ((double)0.0 <= max) {
          if (lhs_min < (double)0.0) {
            type.maybeMinusZero = (bool)1;
          } 
          if (rhs_min < (double)0.0) {
            type.maybeMinusZero = (bool)1;
          }
      }
  }
  // 0 * V8_INFINITY is NaN, regardless of sign
  bool cond1 = (bool)0;
  cond1 |= (lhs_min == -V8_INFINITY);
  cond1 |= (lhs_max == V8_INFINITY);
  bool cond2 = (bool)0;
  cond2 &= (rhs_min <= (double)0.0);
  cond2 &= ((double)0.0 <= rhs_max);
  bool outer1 = cond1 & cond2;
  bool cond3 = (bool)0;
  cond3 |= (rhs_min == -V8_INFINITY);
  cond3 |= (rhs_max == V8_INFINITY);
  bool cond4 = (bool)0;
  cond4 &= (lhs_min <= (double)0.0);
  cond4 &= ((double)0.0 <= lhs_max);
  bool outer2 = cond3 & cond4;
  bool yeet = outer1 | outer2;
  if (yeet) {
      // TODO:
    type.maybeNaN = (bool)1;
  }
  return type;
}

// Precondition: input is numbers
v8type NumberAdd(v8type lhs, v8type rhs) {
  //DCHECK(lhs.Is(Type::Number()));
  //DCHECK(rhs.Is(Type::Number()));

  if (lhs.IsNone() || rhs.IsNone()) return Type::None();

  // Addition can return NaN if either input can be NaN or we try to compute
  // the sum of two infinities of opposite sign.
  bool maybe_nan = lhs.Maybe(Type::NaN()) || rhs.Maybe(Type::NaN());

  // Addition can yield minus zero only if both inputs can be minus zero.
  bool maybe_minuszero = true;
  if (lhs.Maybe(Type::MinusZero())) {
    lhs = Type::Union(lhs, cache_->kSingletonZero, zone());
  } else {
    maybe_minuszero = false;
  }
  if (rhs.Maybe(Type::MinusZero())) {
    rhs = Type::Union(rhs, cache_->kSingletonZero, zone());
  } else {
    maybe_minuszero = false;
  }

  // We can give more precise types for integers.
  Type type = Type::None();
  lhs = Type::Intersect(lhs, Type::PlainNumber(), zone());
  rhs = Type::Intersect(rhs, Type::PlainNumber(), zone());
  if (!lhs.IsNone() && !rhs.IsNone()) {
    if (lhs.Is(cache_->kInteger) && rhs.Is(cache_->kInteger)) {
      type = AddRanger(lhs.Min(), lhs.Max(), rhs.Min(), rhs.Max());
    } else {
      if ((lhs.Maybe(minus_infinity_) && rhs.Maybe(infinity_)) ||
          (rhs.Maybe(minus_infinity_) && lhs.Maybe(infinity_))) {
        maybe_nan = true;
      }
      type = Type::PlainNumber();
    }
  }

  // Take into account the -0 and NaN information computed earlier.
  if (maybe_minuszero) type = Type::Union(type, Type::MinusZero(), zone());
  if (maybe_nan) type = Type::Union(type, Type::NaN(), zone());
  return type;

}
