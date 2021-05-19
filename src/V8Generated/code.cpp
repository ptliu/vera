
#define V8_INFINITY ((double)9218868437227405312)

// oversimplifed V8 one, but meh
// maybeNaN
//
// unions: return any bitset, with maybeNan, maybeMinusZero
struct v8type {
    uint32_t bitset;
    bool hasRange;
    double upper;
    double lower;
    bool maybeNaN;
    bool maybeMinusZero;
    bool isUnion;
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
    type.upper = max;
    type.lower = min;
    type.maybeNaN = (bool)0;
    type.maybeMinusZero = (bool)0;
    type.isUnion = (bool)0;
    return type;
}

v8type nanType() {
    v8type type;
    type.bitset = (uint32_t)0;
    type.hasRange = (bool)0;
    type.upper = max;
    type.lower = min;
    type.maybeNaN = (bool)1;
    type.maybeMinusZero = (bool)0;
    type.isUnion = (bool)0;
    return type;
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
