/* eslint-disable no-param-reassign, class-methods-use-this */
const kthLargestElementInAnArray = () => {
  // I was too lazy to write MinHeap, so I got it here:
  // https://stackoverflow.com/questions/35978242/min-heap-in-javascript
  // for some reason solution with sort works a lot faster than solution with heap.
  // Maybe in tests LeetCode has a lot of cases where k is big.
  class MinHeap<T> {
    private heap: T[];

    constructor(array: T[]) {
      this.heap = this.buildHeap(array);
    }

    // O(n) time | O(1) space
    buildHeap(array: T[]): T[] {
      const firstParentIdx = Math.floor((array.length - 2) / 2);
      for (let currentIdx = firstParentIdx; currentIdx >= 0; currentIdx -= 1) {
        this.siftDown(currentIdx, array.length - 1, array);
      }
      return array;
    }

    // O(log(n)) time | O(1) space
    siftDown(currentIdx: number, endIdx: number, heap: T[]) {
      let childOneIdx = currentIdx * 2 + 1;
      while (childOneIdx <= endIdx) {
        const childTwoIdx =
          currentIdx * 2 + 2 <= endIdx ? currentIdx * 2 + 2 : -1;
        let idxToSwap;
        if (childTwoIdx !== -1 && heap[childTwoIdx] < heap[childOneIdx]) {
          idxToSwap = childTwoIdx;
        } else {
          idxToSwap = childOneIdx;
        }

        if (heap[idxToSwap] < heap[currentIdx]) {
          this.swap(currentIdx, idxToSwap, heap);
          currentIdx = idxToSwap;
          childOneIdx = currentIdx * 2 + 1;
        } else {
          return;
        }
      }
    }

    // O(log(n)) time | O(1) space
    siftUp(currentIdx: number, heap: T[]) {
      let parentIdx = Math.floor((currentIdx - 1) / 2);
      while (currentIdx > 0 && heap[currentIdx] < heap[parentIdx]) {
        this.swap(currentIdx, parentIdx, heap);
        currentIdx = parentIdx;
        parentIdx = Math.floor((currentIdx - 1) / 2);
      }
    }

    // O(1)  time | O(1) space
    peek() {
      return this.heap[0];
    }

    // O(log(n)) time | O(1) space
    remove() {
      this.swap(0, this.heap.length - 1, this.heap);
      const valueToRemove = this.heap.pop();
      this.siftDown(0, this.heap.length - 1, this.heap);
      return valueToRemove;
    }

    // O(log(n)) time | O(1) space
    insert(value: T) {
      this.heap.push(value);
      this.siftUp(this.heap.length - 1, this.heap);
    }

    swap(i: number, j: number, heap: T[]): void {
      [heap[i], heap[j]] = [heap[j], heap[i]];
    }

    size() {
      return this.heap.length;
    }
  }

  const findKthLargest = (nums: number[], k: number): number => {
    return nums.sort((a, b) => b - a)[k - 1];
    // const heap = new MinHeap([]);
    // for (let i = 0; i < nums.length; i += 1) {
    //   if (heap.size() < k || heap.peek() < nums[i]) {
    //     heap.insert(nums[i]);
    //     if (heap.size() > k) {
    //       heap.remove();
    //     }
    //   }
    // }
    // return heap.peek();
  };

  console.log(findKthLargest([3, 2, 1, 5, 6, 4], 2), 5);

  console.log(findKthLargest([3, 2, 3, 1, 2, 4, 5, 5, 6], 4), 4);

  const arr = new Array(10000000)
    .fill(0)
    .map(() => Math.floor(Math.random() * 2 * 10000 - 10000));
  const k = Math.floor(Math.random() * arr.length);
  console.log(findKthLargest(arr, k));
};

kthLargestElementInAnArray();
