#include <stdio.h>
#include <stdlib.h>

int flag1 , flag2;

void dfs ( int n , int m , int fac ){
     if ( flag1== 1 )
           return;

     if ( m==1 && n==1 ) {
           flag1 = 1;
           flag2 = 1;
           return ;
     }

     if ( m==1 )
           flag2 = 1;
     if ( fac<2 ) return ;
     if ( n % fac == 0 )
           dfs ( n/fac, m , fac-1 );
     if ( m%fac== 0 )
           dfs ( n , m/fac, fac-1 );
     dfs ( n , m , fac-1 ) ;
}

int main(){
     int tmp ,n , m ;

     while ( scanf("%d %d",&n,&m)!=EOF ) {
           if ( m>n ) {
                 tmp = m ;
                 m= n ;
                 n = tmp;
           }
           flag1 = flag2 = 0;
           dfs ( n , m , 100 );
           if ( flag1 || !flag2 )
                 printf("%d\n",n);
           else if ( flag2==1&& flag1==0 )
                 printf("%d\n",m);

     }
     return 0;
}
