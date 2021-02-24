/*
Here now we have the code for finding "peek" value of the stack. "Peek" value means
the top-most element present in the Stack.


We have to check for the condition if the stack is empty or not.
*/

int peek(){
	if(isEmpty()){
		printf("Stack is Empty!");
	}
	else{
		return stack[top];
	}
}