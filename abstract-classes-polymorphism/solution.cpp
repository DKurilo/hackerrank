#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <algorithm>
#include <set>
#include <cassert>
using namespace std;

struct Node{
   Node* next;
   Node* prev;
   int value;
   int key;
   Node(Node* p, Node* n, int k, int val):prev(p),next(n),key(k),value(val){};
   Node(int k, int val):prev(NULL),next(NULL),key(k),value(val){};
};

class Cache{
   
   protected: 
   map<int,Node*> mp; //map the key to the node in the linked list
   int cp;  //capacity
   Node* tail; // double linked list tail pointer
   Node* head; // double linked list head pointer
   virtual void set(int, int) = 0; //set function
   virtual int get(int) = 0; //get function

};

class LRUCache : protected Cache {
private:
    void buildWithCapacity(int cp) {
        this->cp = cp;
        head = NULL;
        tail = NULL;
    }
    void moveToHead(Node* n) {
        if (n != head) {
            if (n == tail) {
                tail = n->prev;
                tail->next = NULL;
                n->prev = NULL;
                head->prev = n;
                n->next = head;
                head = n;
            } else {
                n->prev->next = n->next;
                n->next->prev = n->prev;
                n->prev = NULL;
                head->prev = n;
                n->next = head;
                head = n;
            }
        }
    }
public:
    LRUCache() {
        buildWithCapacity(10);
    }
    LRUCache(int cp) {
        buildWithCapacity(cp);
    }
    ~LRUCache() {
        Node *n = head;
        while (1) {
            if (n->next == NULL) {
                delete n;
                break;
            }
            n = n->next;
            delete n->prev;
        }
    }
    void set(int k, int v) {
        map<int,Node*>::iterator it = mp.find(k);
        if (it == mp.end()) {
            if (mp.size() >= cp) {
                mp.erase(tail->key);
                Node* toRemove = tail;
                if (tail == head) {
                    tail = NULL;
                    head = NULL;
                } else {
                    tail = tail->prev;
                    tail->next = NULL;
                }
                delete toRemove;
            }
            if (head == NULL) {
                head = new Node(k, v);
                tail = head;
            } else {
                head = new Node(NULL, head, k, v);
                head->next->prev = head;
            }
            mp.insert(pair<int,Node*>(k, head));
        } else {
            Node* n = it->second;
            moveToHead(n);
            head->value = v;
        }
    }
    int get(int k) {
        map<int,Node*>::iterator it = mp.find(k);
        if (it == mp.end()) {
            return -1;
        }
        Node* n = it->second;
        moveToHead(n);
        return head->value;
    }
};

int main() {
   int n, capacity,i;
   cin >> n >> capacity;
   LRUCache l(capacity);
   for(i=0;i<n;i++) {
      string command;
      cin >> command;
      if(command == "get") {
         int key;
         cin >> key;
         cout << l.get(key) << endl;
      } 
      else if(command == "set") {
         int key, value;
         cin >> key >> value;
         l.set(key,value);
      }
   }
   return 0;
}

