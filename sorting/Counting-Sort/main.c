#include <stdio.h>
#include <stdlib.h>
#define size 1000

// finding maximum number from the array
int maximum(int a[], int n)
{
    int max = a[0];
    for(int  i = 1; i<n; i++)
    {
        if(a[i]>max)
        {
            max = a[i];
        }
    }
    return max;
}


void CountingSort(int a[], int n)
{
    int i, out[n+1], max_num = maximum(a,n), count[max_num+1];

    for(i=0; i<=max_num; ++i)
    {
        count[i] = 0; // initializing count[i] = 0

    }

    for(i = 0; i<n; i++)
    {
        count[a[i]]++; // Storing the count of each elements
    }

    for(int i = 1; i<=max_num; i++)
    {
        count[i] += count[i-1];
    }


    for(i = n-1; i>=0; i--)
    {
        out[count[a[i]-1]] = a[i];
        count[a[i]]--; // decreasing the count of numbers
    }

    for(i = 0; i<n; i++)
    {
        a[i] = out[i]; //storing the stored elements into main array
    }
}

//Printing
void print(int a[], int n)
{
    int i;
    for(i = 0; i<n; i++)
    {
        printf("%d ",a[i]);
    }
}

int main()
{
    int a[size],i,n;
    printf("Enter size: ");
    scanf("%d",&n);

    srand(time(NULL));
    for(i = 0; i<n; i++)
    {
        a[i] = (rand()%100) + 1;
    }
    printf("Elements: ");
    print(a,n);

    //Counting Sort
    CountingSort(a,n);
    printf("\nSorted: ");
    print(a,n);



    return 0;
}

