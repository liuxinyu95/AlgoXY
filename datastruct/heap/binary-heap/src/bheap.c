/*Binary heap with implicit array*/

/*Implicit binary tree mapping in array*/

#define PARENT(i) ((((i) + 1) >> 1) - 1)

#define LEFT(i) (((i)<<1) + 1)

#define RIGHT(i) (((i) + 1) << 1)

#define MIN(x, y) (x < y ? x : y)

#define swap(x, y) { Key temp = x; x = y; y = temp; }

typedef int Key;

void heapify(Key* a, int i, int n) {
    int l, r, m;
    while (1) {
        l = LEFT(i);
        r = RIGHT(i);
        m = i;
        if (l < n && a[l] < a[i])
            m = l;
        if (r < n && a[r] < a[m])
            m = r;
        if (m != i) {
            swap(a[i], a[m]);
            i = m;
        }
        else
            break;
    }
}

void build_heap(Key* a, int n) {
    int i;
    for (i = (n-1) >> 1; i >= 0; --i)
        heapify(a, i, n);
}

Key top(Key* a) { return a[0]; }

Key pop(Key* a, int n) {
    Key x = top(a);
    a[0] = a[--n];
    heapify(a, 0, n);
    return x;
}

/*
 * Find the top k elements
 * in-place put the top k elements in array[0...k-1]
 */
int tops(int k, Key* a, int n) {
    build_heap(a, n);
    for (k = MIN(k, n) - 1; k; --k)
        heapify(++a, 0);
    return k;
}

void heap_fix(Key* a, int i) {
    while (i > 0 && a[i] < a[PARENT(i)]) {
        swap(a[i], a[PARENT(i)]);
        i = PARENT(i);
    }
}

void decrease_key(Key* a, int i, Key k) {
    if (k < a[i]) {
        a[i] = k;
        heap_fix(a, i);
    }
}

/* a[n] should be valid */
void push(Key* a, int n, Key k) {
    a[n] = k;
    heap_fix(a, n);
}

/* in-place sort by performing n heapify */
void heap_sort_slow(Key* a, int n) {
    build_heap(a, n);
    while (--n)
        heapify(++a, 0, n);
}

/* R.W. Floyd heap-sort algorithm */
void heap_sort(Key* a, int n) {
    build_heap(a, n, gt);
    while(n > 1) {
        swap(a[0], a[n--]);
        heapify(a, 0, gt);
    }
}
