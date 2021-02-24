/*The Code written below is to push the element in the Stack.

Write this code below the setup code and also don't forget to define the isFull() fn
outside the main function.

Remember that the element that we will push first will be at the last of the stack.We will see that
when we will write display function to display alll the elements of Stack.
*/

void push(int element){
	if(isFull()){
		printf("Stack is full!");
	}
	else{
		top++;
		stack[top] = element;
	}
}

int isFull(){
	if(top == max-1){
		return 1;
	}
	else{
		return 0;
	}
}