#include <stdlib.h>
#include <vector>

using namespace std;

/*
 * All the methods are based on the fact,
 *      1 <= answer <= n + 1
 *   where n is the length of the array.
 */

/* Flag array method*/

int findMinFree(vector<int> nums) {
    int i, n = nums.size();
    vector<bool> flags(n + 2, false);
    for (i = 0; i < n; ++i)
        if (nums[i] <= n)
            flags[nums[i]] = true;
    for (i = 1; i <= n && flags[i]; ++i);
    return i;
}

/*
 * Radix sort method, put each number to its right
 * position if it's not greater than n
 */

int findMinFree1(vector<int> nums) {
    int i, n = nums.size();
    for (i = 0; i < n; ++i)
        while (nums[i] <= n && nums[i] != i + 1)
            swap(nums[i], nums[nums[i] - 1]);
    for (i = 0; i < n; ++i)
        if (i + 1 != nums[i])
            return i + 1;
    return n + 1;
}

/*
 * Sgn method, reusing the sign for each number to
 * flag the occurrence of the number in that position.
 */

int findMinFree2(vector<int> nums) {
    int i, k, n = nums.size();
    for (i = 0; i < n; ++i)
        if ((k = abs(nums[i])) <= n)
            nums[k - 1] = - abs(nums[k - 1]);
    for (i = 0; i < n; ++i)
        if (nums[i] > 0)
            return i + 1;
    return n + 1;
}

int main() {
    int i, j, k, m, n;
    for (i = 0; i < 100; ++i) {
        n = rand() % 100;
        vector<int> nums(n);
        for (j = 0; j < n; ++j)
            nums[j] = j+1;

        if (n != 0) {
            k = rand() % n;
            for (j = 0; j < k; ++j) {
                m = rand() % n;
                nums[m] = n + rand() % 100 + 1;
            }

            for (j = 0; j < n; ++j)
                swap(nums[j], nums[rand() % n]);
        }

        if ((k = findMinFree(nums)) != (m = findMinFree2(nums))) {
            printf("flag array method: %d\nsng method: %d\n", k, m);
            exit(-1);
        }
        if (k != (m = findMinFree1(nums))) {
            printf("flag array method: %d\nradix soft method: %d\n", k, m);
            exit(-1);
        }
    }
    printf("passed 100 tests\n");
}
