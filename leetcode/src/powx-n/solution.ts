/*
 * well, just:
 * function myPow(x: number, n: number): number {
 *   return x ** n
 * };
 * works fine, but maybe they wanted something more interesting...
 * */
const myPow = (x: number, n: number): number => {
  if (n === 0) {
    return 1;
  }
  if (n < 0) {
    return myPow(1 / x, -n);
  }
  const xpown2 = myPow(x, Math.trunc(n / 2));
  if (n % 2 === 0) {
    return xpown2 * xpown2;
  }
  return x * xpown2 * xpown2;
};

console.log(myPow(2, 10), 2 ** 10);
console.log(myPow(2.1, 3), 2.1 ** 3);
console.log(myPow(2, -2), 2 ** -2);
console.log(myPow(1.00001, 13123), 1.00001 ** 13123);
