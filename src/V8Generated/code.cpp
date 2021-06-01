
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

v8type newBitset(bitset_t bits) {
    v8type type;
    type.bitset = bits;
    type.hasRange = (bool)1;
    type.max = max;
    type.min = min;
    type.maybeNaN = (bool)0;
    type.maybeMinusZero = (bool)0;
    type.isUnion = (bool)0;
    return type;
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

v8type newRange(limits lims){
  return newRange(lims.min, lims.max);
}

v8type nanType() {
    v8type type;
    type.bitset = kNaN;
    type.hasRange = FALSE;
    type.max = DOUBLE_ZERO;
    type.min = DOUBLE_ZERO;
    type.maybeNaN = TRUE;
    type.maybeMinusZero = FALSE;
    type.isUnion = FALSE;
    return type;
}

v8type noneType() {
    v8type type;
    type.bitset = kNone;
    type.hasRange = FALSE;
    type.max = DOUBLE_ZERO;
    type.min = DOUBLE_ZERO;
    type.maybeNaN = FALSE;
    type.maybeMinusZero = FALSE;
    type.isUnion = FALSE;
    return type;
}

v8type minusZeroType() {
    v8type type;
    type.bitset = kMinusZero;
    type.hasRange = FALSE;
    type.max = DOUBLE_ZERO;
    type.min = DOUBLE_ZERO;
    type.maybeNaN = FALSE;
    type.maybeMinusZero = TRUE;
    type.isUnion = FALSE;
    return type;
}

v8type AnyType() {
    v8type type;
    type.bitset = kAny;
    type.hasRange = FALSE;
    type.max = DOUBLE_ZERO;
    type.min = DOUBLE_ZERO;
    type.maybeNaN = TRUE;
    type.maybeMinusZero = TRUE;
    type.isUnion = FALSE;
    return type;
}

v8type minusInfinityType() {
    v8type type;
    type.bitset = kOtherNumber;
    type.max = -V8_INFINITY;
    type.min = -V8_INFINITY;
    type.hasRange = TRUE; 
    type.maybeNaN = FALSE;
    type.maybeMinusZero = FALSE;
    type.isUnion = FALSE;
}

v8type infinityType() {
    v8type type;
    type.bitset = kOtherNumber;
    type.max = V8_INFINITY;
    type.min = V8_INFINITY;
    type.hasRange = TRUE; 
    type.maybeNaN = FALSE;
    type.maybeMinusZero = FALSE;
    type.isUnion = FALSE;
}

v8type plainNumberType() { 
    v8type type;
    type.bitset = kPlainNumber;
    type.hasRange = FALSE; 
    type.maybeNaN = FALSE;
    type.maybeMinusZero = FALSE;
    type.isUnion = FALSE;
}

limits copy(limits other) {
    limits result;
    result.min = other.min;
    result.max = other.max;
    return result;
}

limits getLimits(v8type const& t) {
    limits result;
    result.min = t.min; // TODO: wrong!
    result.max = t.max;
    return result;
}

// boundary helpers

boundary getBoundary(uint32_t index) {
    boundary bound;
    bound.internal = kOtherNumber;
    bound.external = kPlainNumber;
    bound.min = 0.0;

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

//bit helper functions
bool SignedAddWouldOverflow32(int_32t lhs, int_32t rhs){
  uint32_t res = (uint32_t)lhs + (uint32_t)rhs;
  return ((res ^ lhs) & (res ^ rhs) & ((uint32_t) 1 << (uint32_t)31)) != (uint32_t)0;
}

// Range-related helper functions
//

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=22
bool IsEmpty(limits this_) { 
    return this_.min > this_.max;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=24
limits LimitIntersect(limits lhs, limits rhs) {
  //DisallowGarbageCollection no_gc;
  limits result;
  result = copy(lhs);

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
  limits result;
  if (IsEmpty(lhs)) {
      return rhs;
  }
  if (IsEmpty(rhs)) {
      return lhs;
  }
  result = copy(lhs);
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
  return !IsEmpty(LimitIntersect(getLimits(lhs),
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
    return ~(this_.hasRange || this_.isUnion); 
}

bool IsRange(v8type const& this_) {
    return this_.hasRange && !this_.isUnion; 
}

bool IsUnion(v8type const& this_) {
    return this_.isUnion;
}

bool IsTuple(v8type const& this_) {
    return false;
}

bool TypeIsNone(v8type t){
  return BitsetIsNone(t.bitset);
}

bool TypeIsAny(v8type t) {
    return t.bitset == kAny;
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
  //DCHECK(Is(bits, kNumber)); // precond
  //DCHECK(!Is(bits, kNaN)); // precond
  //const Boundary* mins = Boundaries();
  
  bool mz = ((bits & kMinusZero) != (bitset_t)0);

  // DELEGATED TO minBoundary helper, cause we dont got loops...
  // unroll the loop
  boundary minBound = getBoundary((uint32_t)0);
  if (BitsetIs(minBound.internal, bits)) {
    return mz ? math::min((double)0, minBound.min) : minBound.min;
  }
  boundary minBound = getBoundary((uint32_t)1);
  if (BitsetIs(minBound.internal, bits)) {
    return mz ? math::min((double)0, minBound.min) : minBound.min;
  }
  boundary minBound = getBoundary((uint32_t)2);
  if (BitsetIs(minBound.internal, bits)) {
    return mz ? math::min((double)0, minBound.min) : minBound.min;
  }
  boundary minBound = getBoundary((uint32_t)3);
  if (BitsetIs(minBound.internal, bits)) {
    return mz ? math::min((double)0, minBound.min) : minBound.min;
  }
  boundary minBound = getBoundary((uint32_t)4);
  if (BitsetIs(minBound.internal, bits)) {
    return mz ? math::min((double)0, minBound.min) : minBound.min;
  }
  boundary minBound = getBoundary((uint32_t)5);
  if (BitsetIs(minBound.internal, bits)) {
    return mz ? math::min((double)0, minBound.min) : minBound.min;
  }
  boundary minBound = getBoundary((uint32_t)6);
  if (BitsetIs(minBound.internal, bits)) {
    return mz ? math::min((double)0, minBound.min) : minBound.min;
  }

  // TODO: this fails cause mz
  //DCHECK(mz);
  return 0.0;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=453;bpv=0;bpt=1
double BitsetMax(bitset_t bits) {
  //DisallowGarbageCollection no_gc;
  //DCHECK(Is(bits, kNumber));
  //DCHECK(!Is(bits, kNaN));
  
  bool mz = bits & kMinusZero;
  if (BitsetIs(getBoundary((uint32_t)BoundariesSize() - UINT32_ONE).internal, bits)) {
    return V8_INFINITY;
  }

  // unrolled
  /*for (size_t i = BoundariesSize() - 1; i-- > 0;) {
    if (Is(mins[i].internal, bits)) {
      return mz ? std::max(0.0, mins[i + 1].min - 1) : mins[i + 1].min - 1;
    }
  }*/
  uint32_t i = (uint32_t)BoundariesSize() - UINT32_ONE;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+UINT32_ONE);
    return mz ? math::min((double)0, min1.min - UINT32_ONE) : min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+UINT32_ONE);
    return mz ? math::min((double)0, min1.min - UINT32_ONE) : min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+UINT32_ONE);
    return mz ? math::min((double)0, min1.min - UINT32_ONE) : min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+UINT32_ONE);
    return mz ? math::min((double)0, min1.min - UINT32_ONE) : min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+UINT32_ONE);
    return mz ? math::min((double)0, min1.min - UINT32_ONE) : min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+UINT32_ONE);
    return mz ? math::min((double)0, min1.min - UINT32_ONE) : min1.min - UINT32_ONE;
  }
  i -= UINT32_ONE;
  boundary min = getBoundary(i);
  if (BitsetIs(min.internal, bits)) {
    boundary min1 = getBoundary(i+UINT32_ONE);
    return mz ? math::min((double)0, min1.min - UINT32_ONE) : min1.min - UINT32_ONE;
  }

  DCHECK(mz);
  return DOUBLE_ZERO;
}

bitset_t NumberBits(bitset_t bits) {
    return bits & kPlainNumber;
}

// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=418
bitset_t BitsetTypeGlb(double min, double max) {
  bitset_t glb = kNone; //NOTE: Type of glb changed from int(int is weird, why is it signed?)

  // DELEGATED TO minBoundary helper, cause we dont got loops...
  // unroll the loop
  bool continueLoop = TRUE;
  if(max <  (double)-1 || min > DOUBLE_ZERO){
    return glb;
  }

  boundary mins_1 = getBoundary((uint32_t)1);
  
  boundary mins_2 = getBoundary((uint32_t)2);
  if (min < mins_1.min && continueLoop) {
    if(max + DOUBLE_ONE < mins_2.min){
      continueLoop = FALSE;
    }
    glb = glb | mins_1.external;
  }


  boundary mins_3 = getBoundary((uint32_t)3);
  if (min < mins_2.min && continueLoop) {
    if(max + DOUBLE_ONE < mins_3.min){
      continueLoop = FALSE;
    }
    glb = glb | mins_2.external;
  }

  boundary mins_4 = getBoundary((uint32_t)4);
  if (min < mins_3.min && continueLoop) {
    if(max + DOUBLE_ONE < mins_4.min){
      continueLoop = FALSE;
    }
    glb = glb | mins_3.external;
  }

  boundary mins_5 = getBoundary((uint32_t)5);
  if (min < mins_4.min && continueLoop) {
    if(max + DOUBLE_ONE < mins_5.min){
      continueLoop = FALSE;
    }
    glb = glb | mins_5.external;
  }

  boundary mins_6 = getBoundary((uint32_t)6);
  if (min < mins_5.min && continueLoop) {
    if(max + DOUBLE_ONE < mins_6.min){
      continueLoop = FALSE;
    }
    glb = glb | mins_5.external;
  }
  return glb & ~(kOtherNumber);
}


// https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=114
bitset_t BitsetLub(v8type this_) {
  // The smallest bitset subsuming this type, possibly not a proper one.

  // DisallowGarbageCollection no_gc;
  bool unreachable_ = (bool) 0; //TODO: assert that this is false to enforce
                                //code is unreachable at bottom

  if (IsBitset(this_)) {
    return this_.bitset;
  }

  if (IsUnion(this_)) {
    //   // Take the representation from the first element, which is always
    //   // a bitset.
    //   int bitset = AsUnion()->Get(0).BitsetLub();
    //   for (int i = 0, n = AsUnion()->Length(); i < n; ++i) {
    //     // Other elements only contribute their semantic part.
    //     bitset |= AsUnion()->Get(i).BitsetLub();
    //   }
    //   return bitset;
    return this_.bitset;
  }

  // if (IsHeapConstant()) return AsHeapConstant()->Lub();
  // if (IsOtherNumberConstant()) {
  //   return AsOtherNumberConstant()->Lub();
  // }

  if (IsRange(this_)) {
    return this_.bitset;
    // return AsRange()->Lub();
  }

  if (IsTuple(this_)) {
    return kOtherInternal;
  }

  unreachable_ = (bool) 1; //TODO: assert that this is false to enforce 
                           //that this line is never reached
  
  // UNREACHABLE();
}

//https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=96
bitset_t BitsetGlb(v8type this_) {
  if(IsBitset(this_)){
    return AsBitset(this_);
  } else if (IsUnion(this_)){
    //TODO don't handle unions yet
    return AsBitset(this_);
  } else if(IsRange(this_)){
    return BitsetTypeGlb(this_.min, this_.max);
  } else {
    return noneType;
  }
}


// Type methods

// ---
// Ctors
v8type NewBitset(bitset_t bitset) {
    v8type type;
    type.bitset = bitset;
    type.hasRange = FALSE;
    type.max = DOUBLE_ZERO;
    type.min = DOUBLE_ZERO;
    type.maybeNaN = FALSE;
    type.maybeMinusZero = FALSE;
    type.isUnion = FALSE;
    return type;
}

// ----
// Casts

bitset_t AsBitset(v8type this_) {
    return this_.bitset;
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

  if (BitsetIsNone(BitsetLub(this_) & BitsetLub(that))) return false;

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
      bitset_t number_bits = NumberBits(that.bitset);
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

//https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=520
bool SlowIs(v8type this_, v8type that_){
  if(IsBitset(that_)){
    return BitsetIs(BitsetLub(this_), AsBitset(that_));
  }

  if(IsBitset(this_)){
    return BitsetIs(AsBitset(this_), BitsetGlb(that_));
  }

  if(IsUnion(this_)){
    //TODO, for now return true if that_ is a union
    return IsUnion(that_);
  } 
  if(IsUnion(that_)){
    //TODO, for now return true if this_ is union
    //this looks redundant but once we handle unions this will actually do something different
    return IsUnion(this_);
  }
  if(IsRange(that_)){
    return IsRange(this_) && RangeContains(this_, that_);
  }
  if(IsRange(this_)){
    return false;
  }
  return SimplyEquals(this_, that_);
}

//https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.cc;l=48
bool RangeContains(v8type lhs, v8type rhs){
  return lhs.min <= rhs.min && rhs.max <= lhs.max;
}

//https://source.chromium.org/chromium/chromium/src/+/main:v8/src/compiler/types.h;l=394
bool Is(v8type this_, v8type that_){
  //NOTE this is simplified, the first condition here can actually be pointers to complicated
  //types but we don't have that yet
  //TODO: determine whether this matters
  return this_.bitset == that_.bitset || SlowIs(this_, that_);
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
  if (((lhs_min == -V8_INFINITY || lhs_max == V8_INFINITY) &&
       (rhs_min <= 0.0 && 0.0 <= rhs_max)) ||
      ((rhs_min == -V8_INFINITY || rhs_max == V8_INFINITY) &&
       (lhs_min <= 0.0 && 0.0 <= lhs_max))) {
    type.maybeNaN = TRUE;
  }
  return type;
}

limits IntersectAux(v8type lhs, v8type rhs, limits incomingLimit){
  //first two if statements can be ignored since we don't support unions
  if(BitsetIsNone(BitsetLub(lhs) & BitsetLub(rhs))){
    return incomingLimit; 
  }

  if(IsRange(lhs)){
    if(IsBitset(rhs)){

    } 
    if(IsRange(rhs)){
      limits newLimit = LimitIntersect(getLimits(lhs), getLimits(rhs));
      if(!IsEmpty(newLimit)){
        return Union(newLimit, incomingLimit); 
      } else {
        return incomingLimit;
      }
    }
  }
  if(IsRange(rhs)){
    return IntersectAux(rhs, lhs, incomingLimit);
  }

  //last two cases we don't handle because they involve unions again
  //TODO is this correct?
  return incomingLimit;
}

v8type UpdateRange(v8type range, int32_t size){

}

v8type NormalizeUnion(v8type union){ //this is a no-op currently since we don't support unions
  return union;
}

// ignore zone
//Type Type::Intersect(Type type1, Type type2, Zone* zone) {
v8type Intersect(v8type type1, v8type type2) {
  // Fast case: bit sets.
  if (IsBitset(type1) && IsBitset(type2)) {
    return NewBitset(AsBitset(type1) & AsBitset(type2));
  }

  // Fast case: top or bottom types.
  if (TypeIsNone(type1) || TypeIsAny(type2)) return type1;  // Shortcut.
  if (TypeIsNone(type2) || TypeIsAny(type1)) return type2;  // Shortcut.

  // Semi-fast case.
  if (Is(type1, type2)) return type1;
  if (Is(type2, type1)) return type2;

  // Slow case: create union.

  // Semantic subtyping check - this is needed for consistency with the
  // semi-fast case above.
  if (Is(type1, type2)) {
    type2 = Any();
  } else if (Is(type2, type1)) {
    type1 = Any();
  }

 

  bitset_t bits = BitsetGlb(type1) & BitsetGlb(type2);
  int32_t size1 = (int32_t)1; //we don't support unions so this is just 1

  int32_t size2 = (int32_t)1; //also changed from int to int32_t

  if(SignedAddWouldOverflow32(size1, size2)){
    return AnyType();
  }

  int32_t size = size1 + size2;
 
  if(SignedAddWouldOverflow32(size, (int32_t)2)){
    return AnyType();
  }

  v8type result; //was a union but we don't support them :)
  result.bitset = bits;

  size = size + (int32_t)1;

  limits empty = getLimits(noneType());
  limits lims = IntersectAux(type1, type2, empty);
  //update result, normally done in IntersectAux but lack of pointers means we do it this way
  result = newRange(lims);

  if(!IsEmpty(lims)){
    //don't need UpdateRange because we don't support unions
    bitset_t number_bits = NumberBits(bits);
    bits = bits & ~number_bits;
    result.bitset = bits;
  }
  
  return AnyType();
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

v8Type TypeUnion(v8Type type1, v8Type type2) {
  //TODO, need to handle ranges at least a little bit probably
  // Fast case: bit sets.
  if (IsBitset(type1) && IsBitset(type2)) {
    return newBitset(type1.bitset | type2.bitset);
  }

  // Fast case: top or bottom types.
  if (TypeIsAny(type1) || TypeIsNone(type2)) return type1;
  if (TypeIsAny(type2)  || TypeIsNone(type1)) return type2;

  // Semi-fast case.
  if (Is(type1, type2)) return type1;
  if (Is(type2, type1)) return type2;

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
v8type NumberAdd(v8type lhs, v8type rhs) {
  //DCHECK(lhs.Is(Type::Number()));
  //DCHECK(rhs.Is(Type::Number()));

  if (TypeIsNone(lhs) || TypeIsNone(rhs)) {
    return noneType();
  }

  // Addition can return NaN if either input can be NaN or we try to compute
  // the sum of two infinities of opposite sign.
  bool maybe_nan = Maybe(lhs, nanType()) || Maybe(rhs, nanType());

  // Addition can yield minus zero only if both inputs can be minus zero.
  bool maybe_minuszero = true;
  if (Maybe(lhs, minusZeroType())) {
    lhs = TypeUnion(lhs, kSingletonZero); //we don't do unions yet so just merge ranges
  } else {
    maybe_minuszero = false;
  }
  if (Maybe(rhs, minusZeroType())) {
    rhs = TypeUnion(rhs, kSingletonZero);
  } else {
    maybe_minuszero = false;
  }

  // We can give more precise types for integers.
  v8type type = noneType();
  lhs = Intersect(lhs, plainNumberType());
  rhs = Intersect(rhs, plainNumberType());
  if (!TypeIsNone(lhs) && !TypeIsNone(rhs)) {
    if (Is(lhs, kInteger) && Is(rhs, kInteger)) {
      type = AddRanger(lhs.min, lhs.max, rhs.min, rhs.max);
    } else {
      if ((Maybe(lhs, minusInfinityType()) && Maybe(rhs, infinityType())) || // minus_infinity_, infinity_ are Types
          (Maybe(rhs,minusInfinityType()) && Maybe(lhs, infinityType()))) {
        maybe_nan = true;
      }
      type = plainNumberType();
    }
  }

  // Take into account the -0 and NaN information computed earlier.
  if (maybe_minuszero) {
    type = TypeUnion(type, minusZeroType());
  }
  if (maybe_nan) {
    type = TypeUnion(type,nanType());
  }
  return type;
}

