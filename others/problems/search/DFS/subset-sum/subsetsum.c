#include <stdio.h>
#include <stdlib.h>

//convert x from decimal to n digits of m numeric system
void to_digit(int x, int m, int* w, int n){
  int i;
  for(i=0; i<n; ++i){
    w[i] = x % m;
    x = x / m;
  }
}

int power(int x, int n){
  int m=1;
  while(n--)
    m *= x;
  return m;
}

// solve sum(select_from(a)) == s
void solve(int* a, int n, int s){
  int* w = (int*) malloc(n * sizeof(int));
  int i, j, x, m;
  m=power(s+1, n);
  for(i=0; i<m-1; ++i){
    to_digit(i, (s+1), w, n); 
    x = 0;
    for(j=0; j<n; ++j)
      x += a[j]*w[j];
    if(x == s){
      for(j=0; j<n; ++j)
	while(w[j]--) printf("%d, ", a[j]);
      printf("\n");
    }
  }
  free(w);
}

void dfs(int* a, int n, int s){
  int i, j, depth=0;
  int back;
  int* w = (int*) malloc( (s+1)* sizeof(int) );
  int* z = (int*) malloc( (s+1)* sizeof(int) );
  z[depth]=0;
  for(i=0; i<s; ++i)
    w[i] = -1;

  while(depth>=0){
    back = 1;
    for(i=w[depth]+1; i<n; ++i){
      if(a[i]+z[depth] <= s){
	back = 0;
	w[depth] = i;
	z[depth+1] = z[depth]+a[i];
	w[++depth] = -1;
	if(z[depth] == s){ //output 1 solution
	  for(j=0; j<depth; ++j)
	    printf("%d, ", a[w[j]]);
	  printf("\n");
	  back = 1;
	  --depth;
	}
	else
	  break;
      }
    }
    if(back)
      --depth;
  }
  free(z);
  free(w);
}

int main(int argc, char** argv){
  int i, x;
  const int a[] = {1,2,3,4,5,6,7,8,9,10};
  //solve(a, 10, 5);
  dfs(a, 10, 5);
}
