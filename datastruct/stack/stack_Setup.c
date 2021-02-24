/*
STACK CREATION USING STATIC MEMORY ALLOCATION.
---------------------------------------------
Stack works on the rule LIFO i.e. (Last In First Out). It means the element inserted at last will be
poped out first and will be displayed first.
It is similiar to the "Plates Stack" in marriages.
---------------------------------------------

The beolw code is the set-up of Stack creation.
*/

#include<stdio.h>
#include<stdlib.h> 
//Defining the size of Stack by using "#define" so that you can pass the value or write "max" in array called Stack.
#define max 20
int stack[max];
// Initial value of top as -1 because array starts from 0 and we will increase its value at time of insertion.
int top = -1; 

//You have to define all the functions outside the main function.

void push(int element);
int pop();
int display();
int peek();



void main(){
	int element;
	int choice;
	// Apply while loop because it makes easy for us to know what function we want to do repeatedly.
	//This makes understanding easy and our life comfortable :)
	while(1){
		printf("1.Push\n");
		printf("2.Pop\n");
		printf("3.Peek\n");
		printf("4.Display\n");
		printf("5.Quit\n");
		printf("Enter your Choice");
		scanf("%d",&choice);

		// Use switch for the choice you want to perform.
		switch(choice){
			case 1: printf("Enter the element");
					scanf("%d",&element);
					push(element);
			break; 
			case 2: pop();
			break;
			case 3: peek();
			break;
			case 4: display();
			break;
			case 5: exit(0);
			break;
			default: printf("Wrong Choice");
		}
 		//Now We have to define all these functions to perform it on the Stack.
 		//Remember it is the easy way to perform functions in a simplified way. 
 		//So,we will be writing the above code same in all the operations.
	}
}
