/*
t9.c
Dependency : t9.dic
A file with lots of words to populate our t9 trie structure.
All in small letter no spaces no other characters
Terminated by a line with only a 0 (zero)
=================
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct t9{
 struct t9* node[27];
};

struct t9* t9_new(){
 struct t9 * r =  (struct t9 *)malloc(sizeof(struct t9));
 
 int i=0;
 for(;i<27;i++) r->node[i] = (struct t9 *)0;

 return r;
}

void t9_free(struct t9 * root){
 if(root) {
  int i=0;
  for(;i<27;i++)
   t9_free(root->node[i]);
  free(root);
 }
}

struct t9 * t9_insert(struct t9 * root ,char *val){
 if(!root){ root = t9_new(); }

 if(!*val) return root;
 *val |= ('A' ^ 'a');
 char c = *val - 'a';
        root->node[c] = t9_insert(root->node[c] ,++val);

 return root;
}

void t9_print(char *pre, struct t9 * root,int depth){
 int i=0,flag=0;
 for(;i<27;i++) {
  if(root->node[i]){
   pre[depth]='a'+i;flag=1;
   t9_print(pre,root->node[i],depth+1);
   pre[depth]=0;
  }
 }
 if(flag == 0)
 {
  pre[depth]=0;
  printf("%s\n",pre);
 }
}

int in_mob_ks(struct t9* root, char val, int o){
 int a[]={0,3,6,9,12,15,19,22,26};
 /* 2=>0 1 2 
    3=>3 4 5
    4=>6 7 8
    5=>9 10 11
    6=>12 13 14
    7=>15 16 17 18
    8=>19 20 21
    9=>22 23 24 25
  */
 if(o && o>=a[val+1]) return -1;

 int s=o?o:a[val];
 int e=a[val+1];
 //printf("From %d-%d",s,e);
 for(;s<e;s++)
  if(root->node[s])
   return s;
 return -1;
}

void t9_search_mob(char *pre, struct t9 * root,int depth,char *val)
{

 if(*(val+depth)==0)
 {
  pre[depth]=0;
  t9_print(pre,root,depth);
  return;
 }

 int i=in_mob_ks(root,*(val+depth)-'2',0);
 if(i==-1)
 {
  pre[depth]=0;
  //printf("%s\n",pre);
 }
 while(i>=0)
 {
  pre[depth]=i+'a';
  t9_search_mob(pre,root->node[i],depth+1,val);
  pre[depth]=0;
  i=in_mob_ks(root,*(val+depth)-'2',i+1);
 }
}


struct t9 * t9_search(struct t9 * root, char *val)
{
 while(*val)
 {
  if(root->node[*val-'a'])
  {
   root = root->node[*val-'a'];
   val++;
  }
  else return NULL;
 }
 return root;
}

int main()
{
 struct t9 * root = (struct t9 *) 0;
 char a[100],b[100];int i;
 FILE *fp = fopen("t9.dic","r");
 while(!feof(fp))
 {
  fscanf(fp,"%s",&a);
  if(a[0]=='0')break;
  root=t9_insert(root,a);
 }
 
 while(1)
 {
  printf("mob keys 2-9:");
  scanf("%s",&a);
  if(a[0]=='0')break;
  t9_search_mob(b,root,0,a);
 }
 t9_free(root);
}
