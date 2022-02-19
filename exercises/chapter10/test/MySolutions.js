"use strict";

// (Medium) Write a JavaScript function volumeFn in the Test.MySolutions module
// that finds the volume of a box. Use an Fn wrapper from Data.Function.Uncurried.

exports.volumeFn = function (l, w, h) {
  return l * w * h;
};

// (Medium) Rewrite volumeFn with arrow functions as volumeArrow.

exports.volumeArrow = (l) => (w) => (h) => l * w * h;

// (Medium) Write a JavaScript function cumulativeSumsComplex (and corresponding
// PureScript foreign import) that takes an Array of Complex numbers and returns
// the cumulative sum as another array of complex numbers.

exports.cumulativeSumsComplex = (arr) => {
  let complexSums = [];
  let complexSum = {
    real: 0,
    imag: 0,
  };
  arr.forEach((x) => {
    complexSum = {
      real: x.real + complexSum.real,
      imag: x.imag + complexSum.imag,
    };
    complexSums.push(complexSum);
  });
  return complexSums;
};

// (Medium) Write the function toMaybe :: forall a. Undefined a -> Maybe a. This
// function converts undefined to Nothing and a values to Justs.

exports.toMaybeImpl = (just) => (nothing) => (pUndefined) => {
  if (pUndefined === undefined) return nothing;
  else return just(pUndefined);
};

// (Medium) Write a JavaScript function and PureScript wrapper valuesOfMap :: Map
// String Int -> Either JsonDecodeError (Set Int) that returns a Set of all the
// values in a Map. Hint: The .values() instance method for Map may be useful in
// your JavaScript code.

exports.valuesOfMapJson = (j) => {
  let m = new Map(j);
  let s = new Set(m.values());
  return Array.from(s);
};
