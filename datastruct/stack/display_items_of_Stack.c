/*
now comes the display code...
whatever we will do whether push,pop or finding peek value we have to display it so that we or the user can see it.

So look at the code below:

*/

int display(){
	if(isEmpty()){
		printf("Stack is Empty!")
	}
	else{
		for(int i=0;i<=top;i++){
			printf("%d\n",stack[i]);
		}
	}
}


/*
you can also display the elements in the reverse position.
Just change the loop.
*/

int display(){
	if(isEmpty()){
		printf("Stack is Empty!")
	}
	else{
		for(int i=top;i>0;i--){
			printf("%d\n",stack[i]);
		}
	}
}
