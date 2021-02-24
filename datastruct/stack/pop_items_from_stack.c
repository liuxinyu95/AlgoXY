/*
The "pop" function is used to delete the elements from the stack.
To delete it we have to check all the possible ways like if it is already empty or not. So,
here is the code to pop the elements from a stack.

Write this code below the setup code and also don't forget to define the isEmpty() fn
outside the main function.

*/

int pop(){
	int element;
	if(isEmpty()){
		printf("Stack is Empty");
	}
	else{
		element = stack[top];
		top--;
	}
	return top;
}

int isEmpty(){
	if(top == -1){
		return 1;
	}
	else{
		return 0;
	}
}