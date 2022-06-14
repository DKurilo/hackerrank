type Maybe<T> =
  | {
      tag: 'Nothing';
    }
  | {
      tag: 'Just';
      value: T;
    };

const nothing = <T>(): Maybe<T> => ({ tag: 'Nothing' });

const just = <T>(val: T): Maybe<T> => ({ tag: 'Just', value: val });

const isNothing = <T>(mb: Maybe<T>): boolean => mb.tag === 'Nothing';

const fromMaybe = <T>(def: T, mb: Maybe<T>) =>
  mb.tag === 'Nothing' ? def : mb.value;

type DoerState = {
  queue: number[];
  used: Record<number, number>;
  sum: Maybe<number>;
  currentSum: number;
  offset: number;
};

const mkDoerState = (): DoerState => ({
  queue: [],
  used: {},
  sum: nothing(),
  currentSum: 0,
  offset: 0,
});

const doerMUS = (st: DoerState, n: number, k: number): DoerState => {
  if (st.used[n] !== undefined) {
    const newOffset = st.used[n] + 1;
    const i = newOffset - st.offset;
    const newQueue = st.queue.slice(i);
    newQueue.push(n);
    let newCurrentSum = st.currentSum;
    for (let j = 0; j < i - 1; j += 1) {
      newCurrentSum -= st.queue[j];
      // eslint-disable-next-line no-param-reassign
      st.used[st.queue[j]] = undefined;
    }
    // eslint-disable-next-line no-param-reassign
    st.used[n] = k;
    return {
      queue: newQueue,
      used: st.used,
      sum:
        isNothing(st.sum) || st.currentSum > fromMaybe(0, st.sum)
          ? just(st.currentSum)
          : st.sum,
      currentSum: newCurrentSum,
      offset: newOffset,
    };
  }
  // eslint-disable-next-line no-param-reassign
  st.used[n] = k;
  st.queue.push(n);
  return {
    queue: st.queue,
    used: st.used,
    sum: st.sum,
    currentSum: st.currentSum + n,
    offset: st.offset,
  };
};

const maximumUniqueSubarray = (nums: number[]): number => {
  const st = nums.reduce(doerMUS, mkDoerState());
  return Math.max(fromMaybe(-1, st.sum), st.currentSum);
};

console.log(
  maximumUniqueSubarray([
    62,
    23,
    62,
    60,
    51,
    49,
    57,
    34,
    88,
    6,
    67,
    101,
    34,
    84,
    19,
    100,
    75,
    20,
    3,
    47,
    71,
    23,
    48,
    47,
    28,
    87,
    58,
    96,
    36,
    42,
  ]),
);
console.log(maximumUniqueSubarray([4, 2, 4, 5, 6]), 17);
console.log(maximumUniqueSubarray([5, 2, 1, 2, 5, 2, 1, 2, 5]), 8);
const testArr = new Array(100000)
  .fill(0)
  .map(() => Math.ceil(Math.random() * 10000 + 1));
console.log(maximumUniqueSubarray(testArr));
const testArr1 = new Array(100000).fill(0).map((_, i) => i);
console.log(maximumUniqueSubarray(testArr1));
const testArr2 = new Array(100000).fill(0).map((_, i) => i % 10000);
console.log(maximumUniqueSubarray(testArr2));
