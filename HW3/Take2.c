#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Node
{
    int value;
    struct Node *next;
    struct Node *prev;
};

struct DLinkedList
{
    struct Node *head;
    struct Node *tail;
    struct Node *root;
};


void add_to_front(struct DLinkedList *list, int value)
{
    struct Node *node = malloc( sizeof(struct Node) );
    node-> value = value;
    list->head->prev = node;
    node->next = list->head;
    list->head = node;
                                //printf("%d\n", list->head->value);

};
void add_to_back(struct DLinkedList *list, int value)
{
    struct Node *node = malloc( sizeof(struct Node) );
    node-> value = value;
    list->tail->next =node;
    node->prev = list->tail;
    list->tail = node;

};

int remove_from_front(struct DLinkedList *list)
{
    struct Node *newHead = list->head->next;
    int x = list->head->value;
    free(list->head);
    list->head = newHead;
    return x;
};
int remove_from_back(struct DLinkedList *list)
{

    struct Node *newTail = list->tail->prev;
    int x = list->tail->value;
    free(list->tail);
    list->tail = newTail;
    return x;
};
struct DLinkedList transfer(int arrayIn[], int arrayOut[], int length, int (*fPtr1)(struct DLinkedList *list), void (*fPtr2)(struct DLinkedList *list, int x))
{
        struct DLinkedList *list = malloc(sizeof(struct DLinkedList));
        struct Node *node = malloc(sizeof(struct Node));
        node->value = arrayIn[0];
        list->head = node;
        list->tail = node;
        int x;
        for(x = 1; x < length; x++)
        {
            fPtr2(list, arrayIn[x]);
            //printf("%d\n", list->head->value);

        }
        for (x = 0; x < length; x++)
        {
            arrayOut[x] = fPtr1(list);
                //printf("%d\n", list->tail->value);

        }


};

int main()
{


    void (*ptrAdd)(struct DLinkedList,int);
    int (*ptrRemove)(struct DLinkedList);
    ptrAdd = &add_to_front;
    ptrRemove = &remove_from_front;

    int array1[4] = {1, 2, 3 ,4};
    int array2[4];
    transfer(array1, array2, 4, ptrRemove, ptrAdd);
    int x;
    for (x = 0; x < 4; x++)
    {
        printf("%d\n", array2[x]); //passed

    }
    ptrAdd = &add_to_back;
    ptrRemove = &remove_from_front;
    int array3[4] = {1, 2, 3 ,4};
    int array4[4];
    transfer(array3, array4, 4, ptrRemove, ptrAdd);
    for (x = 0; x < 4; x++)
    {
        printf("%d\n", array4[x]); //passed

    }
    ptrAdd = &add_to_front;
    ptrRemove = &remove_from_back;
    int array5[4] = {1, 2, 3 ,4};
    int array6[4];
    transfer(array5, array6, 4, ptrRemove, ptrAdd);
    for (x = 0; x < 4; x++)
    {
        printf("%d\n", array6[x]); //passed
    }

    ptrAdd = &add_to_back;
    ptrRemove = &remove_from_back;
    int array7[4] = {1, 2, 3 ,4};
    int array8[4];
    transfer(array7, array8, 4, ptrRemove, ptrAdd);
    for (x = 0; x < 4; x++)
    {
        printf("%d\n", array8[x]); //passed
    }


    return 0;
};
