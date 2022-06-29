function maxSlidingWindow(nums: number[], k: number): number[] {
  return nums.reduce(
    ({ q, max, w }, n) => {
      if (q.length < k) {
        q.push(n);
        if (q.length === k) {
          const mx = Math.max(...q);
          w.push(mx);
          return { q, max: mx, w };
        }
        return { q, max, w };
      }
      const nl = q.shift();
      q.push(n);
      if (n >= max) {
        w.push(n);
        return { q, max: n, w };
      }
      if (nl === max) {
        const mx = Math.max(...q);
        w.push(mx);
        return { q, max: mx, w };
      }
      w.push(max);
      return { q, max, w };
    },
    { q: [], max: null, w: [] },
  ).w;
}

console.log(maxSlidingWindow([1, 3, -1, -3, 5, 3, 6, 7], 3), [
  3,
  3,
  5,
  5,
  6,
  7,
]);
console.log(maxSlidingWindow([1], 1), [1]);
console.log(maxSlidingWindow([1, -1], 1), [1]);
