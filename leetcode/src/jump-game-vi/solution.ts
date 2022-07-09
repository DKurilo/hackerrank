const jumpGameVI = () => {
  class MaxHeap {
    private tree: [number, number][] = [];

    private static parent(i: number): number {
      return Math.floor((i - 1) / 2);
    }

    private static gt(el1: [number, number], el2: [number, number]): boolean {
      if (el1[0] === el2[0]) {
        return el1[1] > el2[1];
      }

      return el1[0] > el2[0];
    }

    private heapify(i: number): void {
      const left = 2 * i + 1;
      let largest = i;
      const right = 2 * i + 2;
      if (
        left < this.tree.length &&
        MaxHeap.gt(this.tree[left], this.tree[largest])
      ) {
        largest = left;
      }
      if (
        right < this.tree.length &&
        MaxHeap.gt(this.tree[right], this.tree[largest])
      ) {
        largest = right;
      }
      if (largest !== i) {
        const temp = this.tree[largest];
        this.tree[largest] = this.tree[i];
        this.tree[i] = temp;
        this.heapify(largest);
      }
    }

    replace(el: [number, number]) {
      const res = this.tree[0];
      this.tree[0] = el;
      this.heapify(0);
      return res;
    }

    insert(el: [number, number]): void {
      this.tree.push(el);
      let i = this.tree.length - 1;
      while (i > 0 && MaxHeap.gt(this.tree[i], this.tree[MaxHeap.parent(i)])) {
        const temp = this.tree[i];
        this.tree[i] = this.tree[MaxHeap.parent(i)];
        this.tree[MaxHeap.parent(i)] = temp;
        i = MaxHeap.parent(i);
      }
    }

    deleteMax(): void {
      const el = this.tree.pop();
      this.tree[0] = el;
      this.heapify(0);
    }

    findMax() {
      return this.tree[0];
    }
  }

  const maxResult = (nums: number[], k: number): number => {
    if (k === 1) {
      return nums.reduce((a, b) => a + b, 0);
    }
    const jumps: number[] = new Array(nums.length).fill(-Infinity);
    jumps[nums.length - 1] = nums[nums.length - 1];
    const heap = new MaxHeap();
    heap.insert([nums[nums.length - 1], nums.length - 1]);
    for (let i = nums.length - 2; i >= 0; i -= 1) {
      const maxJump = heap.findMax();
      jumps[i] = nums[i] + maxJump[0];
      while (heap.findMax()[0] > jumps[i] && i + k <= heap.findMax()[1]) {
        heap.deleteMax();
      }
      heap.insert([jumps[i], i]);
    }
    return jumps[0];
  };

  const maxResult_dequeue = (nums: number[], k: number): number => {
    const dq: number[] = [];

    for (let i = nums.length - 1; i >= 0; i -= 1) {
      while (dq.length > 0 && dq[0] > i + k) dq.shift();
      if (dq.length <= 0) dq.push(i);
      else {
        // eslint-disable-next-line no-param-reassign
        nums[i] = nums[dq[0]] + nums[i];
        while (dq.length > 0 && nums[dq[dq.length - 1]] < nums[i]) dq.pop();
        dq.push(i);
      }
    }
    return nums[0];
  };

  console.log(maxResult([1, -1, -2, 4, -7, 3], 2), 7);

  console.log(maxResult([10, -5, -2, 4, 0, 3], 3), 17);

  console.log(maxResult([1, -5, -20, 4, -1, 3, -6, -3], 2), 0);

  console.log(maxResult([-1, -2, -100, -3, -40, -5], 2), -11);

  console.log(maxResult([-1, -2, -3, -4, -5, -6], 2), -13);
};

jumpGameVI();
