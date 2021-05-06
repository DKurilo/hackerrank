// node js still can't tail calls optimization, so I wasn't able to solve it with functional programming approach. :(
class ListNode {
  val: number;

  next: ListNode | null;

  constructor(val?: number, next?: ListNode | null) {
    this.val = val === undefined ? 0 : val;
    this.next = next === undefined ? null : next;
  }
}

type ListNodeF = ListNode | null;

const arrToListNode = (xs: number[]): ListNodeF => {
  let ln = null;
  for (let i = 0; i < xs.length; i += 1) {
    ln = new ListNode(xs[xs.length - i - 1], ln);
  }
  return ln;
};

const listNodeToArr = (ln: ListNodeF): number[] => {
  let x = ln;
  const xs = [];
  while (x !== null) {
    xs.push(x.val);
    x = x.next;
  }
  return xs;
};

const compose = <A, B, C>(f: (x: B) => C, g: (x: A) => B) => (x: A): C =>
  f(g(x));

const minBy = <T>(f: (a: T, b: T) => number, xs: T[]): T =>
  xs.reduce((min, x) => (f(min, x) <= 0 ? min : x), xs[0]);

const reverse = (x: ListNodeF): ListNodeF => {
  let ln = null;
  let y = x;
  while (y !== null) {
    ln = new ListNode(y.val, ln);
    y = y.next;
  }
  return ln;
};

/**
 * Definition for singly-linked list.
 * class ListNode {
 *     val: number
 *     next: ListNode | null
 *     constructor(val?: number, next?: ListNode | null) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.next = (next===undefined ? null : next)
 *     }
 * }
 */

const mergeKListsDoer = (lists: ListNode[]): ListNodeF => {
  let lns = lists.slice();
  let ln = null;
  while (lns.length > 0) {
    const notEmptyLists = lns.filter((l) => l !== null);
    if (notEmptyLists.length === 0) {
      return ln;
    }
    const min = minBy(
      (a, b) => notEmptyLists[a].val - notEmptyLists[b].val,
      notEmptyLists.map((_, i) => i),
    );
    ln = new ListNode(notEmptyLists[min].val, ln);
    lns = notEmptyLists.map((l, i) => (i === min ? l.next : l));
  }
  return ln;
};

const mergeKLists = compose(reverse, mergeKListsDoer);

console.log(
  listNodeToArr(
    mergeKLists(
      [
        [1, 4, 5],
        [1, 3, 4],
        [2, 6],
      ].map(arrToListNode),
    ),
  ),
);
console.log(listNodeToArr(mergeKLists([].map(arrToListNode))));
console.log(listNodeToArr(mergeKLists([[]].map(arrToListNode))));

const arrsCompare = <T>(xs: T[], ys: T[]): boolean => {
  if (xs.length !== ys.length) {
    return false;
  }
  for (let i = 0; i < xs.length; i += 1) {
    if (xs[i] !== ys[i]) {
      return false;
    }
  }
  return true;
};
const arrs = [...new Array(100)].map(() =>
  [...new Array(10000)]
    .map(() => Math.trunc(Math.random() * 1000000))
    .sort((a, b) => a - b),
);
const sorted = arrs.flat().sort((a, b) => a - b);
console.log(
  arrsCompare(listNodeToArr(mergeKLists(arrs.map(arrToListNode))), sorted),
);
