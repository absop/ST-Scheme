#include <stdio.h>
#include <stdlib.h>

typedef struct _list {
    int data;
    struct _list *next;
} list_t;

#define sky_new(type) (type*)malloc(sizeof(type))

list_t* cons(int x, list_t *tail)
{
    list_t *head = sky_new(list_t);
    head->data = x;
    head->next = tail;
    return head;
}

list_t *reverse(list_t* head)
{
    list_t *prev, *next;
    if (head) {
        next = head->next;
        head->next = NULL;
        while (next) {
            prev = head;
            head = next;
            next = next->next;
            head->next = prev;
        }
    }
    return head;
}

void print(list_t *head)
{
#if 1
    for (; head; head = head->next) {
        printf("%d -> ", head->data);
    }
    printf("%s\n", "null");
#else
    printf("[");
    if (head) {
        printf("%d", head->data);
        while ( head = head->next ) {
            printf(", %d", head->data);
        }
    }
    printf("]\n");
#endif
}

int main()
{
    print(NULL);
    print(reverse(NULL));

    list_t *list1 = cons(1, NULL);
    print(list1);
    print(reverse(list1));

    list_t *list2 = cons(1, cons(2, NULL));
    print(list2);
    print(reverse(list2));

    list_t *list3 = cons(1, cons(2, cons(3, NULL)));
    print(list3);
    print(reverse(list3));

    list_t *list6 = cons(1, cons(2, cons(3, cons(4, cons(5, cons(6, NULL))))));
    print(list6);
    print(reverse(list6));

    return 0;
}
